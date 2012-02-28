module Main ( main ) where

import qualified Distribution.Hackage.DB as DB
import Distribution.NixOS.Derivation.Cabal
import Distribution.NixOS.Scan

import Control.Applicative
import Control.Exception ( bracket )
import Control.Monad
import Control.Monad.RWS

import Data.Foldable ( for_ )
import Data.List
import Data.Monoid
import qualified Data.Map as Map

import System.Console.GetOpt 
import System.Environment
import System.IO

type KnownPkgs = Map.Map String Pkg

type FindExistingPkgs a = RWST Configuration () KnownPkgs IO a

findExistingPkgs :: FilePath -> FindExistingPkgs ()
findExistingPkgs nixpkgsDir = do
    msgDebug $ "locating all cabal packages in " ++ nixpkgsDir
    flip withNixFiles nixpkgsDir $ \path -> do
      let notePackageExists _ deriv = modify $ Map.insert (pname deriv) (Pkg deriv path undefined)
      liftIO (readFile path) >>= onHaskellDerivation path notePackageExists

type AddFromHackage a = RWST Configuration [Pkg] KnownPkgs IO a

addPkgsFromHackage :: FilePath -> String -> AddFromHackage ()
addPkgsFromHackage nixpkgsDir pkgName = do
    msgDebug $ "cabal package " ++ pkgName ++ " ..."
    alreadyExists <- gets (Map.member pkgName)
    case alreadyExists of
        True -> do
            msgDebug "... exists."
            return ()
        False -> do
            msgDebug "... does not exist."
            let pkg = Pkg 
            tell [ pkgName ]
            return ()
                   
    
data Opts = DryRun | Verbose
    deriving ( Show, Eq )

-- No options for AddFromHackage yet.
optDescriptions = [ Option "n" ["dry-run"] (OptArg ( const DryRun ) "") "Dry run" 
                  , Option "v" ["verbose"] (OptArg ( const Verbose ) "") "Verbose"
                  ]

-- given the name of a cabal package and a nix derivation directory:
--  \name -> if a cabal derivation is not found with the given name then 
--      add the cabal derivation from hackage
--      for each build depend of the cabal derivation
--          recurse with name and max dependent version of build depend
--
-- XXX: You still need to add the packages to haskell_packages.nix afterwards.
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
    -- parse the command line args
    ( opts, nixpkgsDir : pkgToAdd : _, optErrors ) <- getOpt RequireOrder optDescriptions <$> getArgs
    when (not $ null optErrors ) $ fail $ intercalate "\n" optErrors

    -- init the hackage DB
    hackage <- DB.readHackage
    let cfg = defaultConfiguration
            { _msgDebug  = if Verbose `elem` opts then _msgDebug defaultConfiguration else const (return ())
            , _hackageDb = hackage
            }

    -- find all existing packages
    ( (), existingPkgs, () ) <- runRWST (findExistingPkgs nixpkgsDir) cfg Map.empty

    liftIO $ do
        _msgDebug cfg "existing packages:"
        for_ existingPkgs $ \(Pkg deriv _ _) -> _msgDebug cfg  $ "    " ++ pname deriv

    -- find all packages to add.
    ( (), _, allPkgsToAdd ) <- runRWST (addPkgsFromHackage nixpkgsDir pkgToAdd ) cfg existingPkgs

    -- print them out
    liftIO $ do
        _msgInfo cfg $ "will add packages:"
        for_ allPkgsToAdd $ \(Pkg deriv out_path _) -> do
            _msgInfo cfg $ "    " ++ pname deriv ++ " => " ++ out_path

    -- add them if not a dry run
    case DryRun `elem` opts of
        False -> fail "only dry run supported"
        True  -> return ()
