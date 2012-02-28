module Main ( main ) where

import Cabal2Nix.Normalize
import Cabal2Nix.Generate
import Control.Exception ( bracket )
import Control.Monad.RWS
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.Version
import qualified Distribution.Hackage.DB as DB
import Distribution.NixOS.Derivation.Cabal
import Distribution.NixOS.Scan
import Distribution.Package
import Distribution.Text
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..), ArgOrder(..), usageInfo, getOpt )
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Text.Regex.Posix

type Hackage4Nix a = RWST Configuration () PkgSet IO a

io :: (MonadIO m) => IO a -> m a
io = liftIO

regenerateDerivation :: Derivation -> String -> Bool
regenerateDerivation deriv buf = not (pname deriv `elem` patchedPackages) &&
                                 not (buf =~ "(pre|post)Configure|(pre|post)Install|patchPhase|patches")

selectLatestVersions :: PkgSet -> PkgSet
selectLatestVersions = Set.fromList . nubBy f2 . sortBy f1 . Set.toList
  where
    f1 (Pkg deriv1 _ _) (Pkg deriv2 _ _)
      | pname deriv1 == pname deriv2     = compare (version deriv2) (version deriv1)
      | otherwise                        = compare (pname deriv1) (pname deriv2)
    f2 (Pkg deriv1 _ _) (Pkg deriv2 _ _) = pname deriv1 == pname deriv2

discoverUpdates :: String -> Version -> Hackage4Nix [Version]
discoverUpdates name vers = do
  db <- asks _hackageDb
  let versions = DB.keys (fromJust (DB.lookup name db))
  return (filter (>vers) versions)

updateNixPkgs :: [FilePath] -> Hackage4Nix ()
updateNixPkgs paths = do
  msgDebug $ "updating = " ++ show paths
  flip mapM_ paths $ \fileOrDir ->
    flip withNixFiles fileOrDir $ \path -> do
      let makeUpdatedPkg buf deriv = return $ First $ Just $ Pkg deriv path (regenerateDerivation deriv buf)
      mnix <- io (readFile path) >>= onHaskellDerivation path makeUpdatedPkg 
      flip (maybe (return ())) (getFirst mnix) $ \nix -> do
        let Pkg deriv _ regenerate = nix
            maints = maintainers (metaSection deriv)
            plats  = platforms (metaSection deriv)
        modify (Set.insert nix)
        when regenerate $ do
          msgDebug ("re-generate " ++ path)
          pkg <- getCabalPackage (pname deriv) (version deriv)
          let deriv'  = (cabal2nix pkg) { sha256 = sha256 deriv, runHaddock = runHaddock deriv }
              meta    = metaSection deriv'
              plats'  = if null plats then platforms meta else plats
              deriv'' = deriv' { metaSection = meta
                                               { maintainers = maints ++ ["simons","andres"]
                                               , platforms   = plats'
                                               }
                               }
          io $ writeFile path (show (disp (normalize (deriv''))))
  pkgset <- gets selectLatestVersions
  updates' <- flip mapM (Set.elems pkgset) $ \pkg -> do
    let Pkg deriv _ _ = pkg
    updates <- discoverUpdates (pname deriv) (version deriv)
    return (pkg,updates)
  let updates = [ u | u@(_,(_:_)) <- updates' ]
  when (not (null updates)) $ do
    msgInfo "The following updates are available:"
    flip mapM_ updates $ \(pkg,versions) -> do
      let Pkg deriv path regenerate = pkg
      msgInfo ""
      msgInfo $ (display (packageId deriv)) ++ ":"
      flip mapM_ versions $ \newVersion -> do
        let deriv' = deriv { version = newVersion }
        msgInfo $ "  " ++ genCabal2NixCmdline (Pkg deriv' path regenerate)
  return ()

genCabal2NixCmdline :: Pkg -> String
genCabal2NixCmdline (Pkg deriv path _) = unwords $ ["cabal2nix"] ++ opts ++ [">"++path']
  where
    meta = metaSection deriv
    opts = [cabal] ++ maints' ++ plats' ++ if runHaddock deriv then [] else ["--no-haddock"]
    cabal = "cabal://" ++ display (packageId deriv)
    maints' = [ "--maintainer=" ++ normalizeMaintainer m | m <- maintainers meta ]
    plats'
      | ["self.ghc.meta.platforms"] == platforms meta = []
      | otherwise                                     = [ "--platform=" ++ p | p <- platforms meta ]
    path'
      | path =~ "/[0-9\\.]+\\.nix$" = replaceFileName path (display (version deriv) <.> "nix")
      | otherwise                   = path

normalizeMaintainer :: String -> String
normalizeMaintainer x
  | "self.stdenv.lib.maintainers." `isPrefixOf` x = drop 28 x
  | otherwise                                     = x

data CliOption = PrintHelp | Verbose
  deriving (Eq)

main :: IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
  let options :: [OptDescr CliOption]
      options =
        [ Option ['h'] ["help"]     (NoArg PrintHelp)                 "show this help text"
        , Option ['v'] ["verbose"]  (NoArg Verbose)                   "enable noisy debug output"
        ]

      usage :: String
      usage = usageInfo "Usage: hackage4nix [options] [dir-or-file ...]" options ++ unlines
              [ ""
              , "The purpose of 'hackage4nix' is to keep all Haskell packages in our"
              , "repository packages up-to-date. It scans a checked-out copy of"
              , "Nixpkgs for expressions that use 'cabal.mkDerivation', and"
              , "re-generates them in-place with cabal2nix."
              ]

      cmdlineError :: String -> IO a
      cmdlineError ""     = hPutStrLn stderr usage >> exitFailure
      cmdlineError errMsg = hPutStrLn stderr errMsg >> cmdlineError ""

  args' <- getArgs
  (opts,args) <- case getOpt Permute options args' of
     (o,n,[]  ) -> return (o,n)
     (_,_,errs) -> cmdlineError (concatMap (\e -> '*':'*':'*':' ':e) errs)

  when (PrintHelp `elem` opts) (cmdlineError "")

  hackage <- DB.readHackage
  let cfg = defaultConfiguration
            { _msgDebug  = if Verbose `elem` opts then _msgDebug defaultConfiguration else const (return ())
            , _hackageDb = hackage
            }
  ((),_,()) <- runRWST (updateNixPkgs args) cfg Set.empty
  return ()

