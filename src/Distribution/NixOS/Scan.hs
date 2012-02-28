{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Distribution.NixOS.Scan where

import qualified Distribution.Hackage.DB as DB
import Distribution.NixOS.Derivation.Cabal

import Control.Monad.IO.Class
import Control.Monad.RWS.Class

import Data.List
import Data.Monoid
import qualified Data.Set as Set

import Distribution.PackageDescription ( GenericPackageDescription() )
import Distribution.Text

import System.Directory
import System.FilePath
import System.IO

import Text.Regex.Posix

-- XXX: Using the full Configuration structure imposes a larger requirement on the user than just
-- requiring an appropriate type class.

data Configuration = Configuration
  { _msgDebug  :: String -> IO ()
  , _msgInfo   :: String -> IO ()
  , _hackageDb :: DB.Hackage
  }

defaultConfiguration :: Configuration
defaultConfiguration = Configuration
  { _msgDebug  = hPutStrLn stderr
  , _msgInfo   = hPutStrLn stderr
  , _hackageDb = DB.empty
  }

readDirectory :: FilePath -> IO [FilePath]
readDirectory dirpath = do
  entries <- getDirectoryContents dirpath
  return [ x | x <- entries, x /= ".", x /= ".." ]

msgDebug, msgInfo :: ( MonadIO m, MonadReader Configuration m ) => String -> m ()
msgDebug msg = ask >>= \s -> liftIO (_msgDebug s msg)
msgInfo msg = ask >>= \s -> liftIO (_msgInfo s msg)

getCabalPackage :: MonadReader Configuration m => String -> Version -> m GenericPackageDescription
getCabalPackage name vers = do
  db <- asks _hackageDb
  case DB.lookup name db of
    Just db' -> case DB.lookup vers db' of
                  Just pkg -> return pkg
                  Nothing  -> fail ("hackage doesn't know about " ++ name ++ " version " ++ display vers)
    Nothing  -> fail ("hackage doesn't know about " ++ show name)

data Pkg = Pkg Derivation FilePath Bool
  deriving (Show, Eq, Ord)

type PkgSet = Set.Set Pkg

-- Packages that we cannot parse.

badPackagePaths :: [FilePath]
badPackagePaths = ["haskell-platform/2011.2.0.1.nix","haskell-platform/2011.4.0.0.nix"]

-- Packages that we cannot regenerate automatically yet. This list
-- should be empty.

patchedPackages :: [String]
patchedPackages = []

onHaskellDerivation :: forall w s m a . ( MonadRWS Configuration w s m, MonadIO m, Monoid a ) 
                                      => FilePath 
                                      -> ( String -> Derivation -> m a )
                                      -> String 
                                      -> m a 
onHaskellDerivation path f buf
  | not (buf =~ "cabal.mkDerivation")
               = msgDebug ("ignore non-cabal package " ++ path) >> return mempty
  | any (`isSuffixOf`path) badPackagePaths
               = msgDebug ("ignore known bad package " ++ path) >> return mempty
  | buf =~ "src = (fetchurl|fetchgit|sourceFromHead)"
               = msgDebug ("ignore non-hackage package " ++ path) >> return mempty
  | Just deriv <- parseDerivation buf = f buf deriv
  | otherwise = msgInfo ("failed to parse file " ++ path) >> return mempty

withNixFiles :: forall w s m . ( MonadRWS Configuration w s m, MonadIO m ) 
                               =>  ( FilePath -> m () ) 
                               -> FilePath 
                               -> m ()
withNixFiles yield dirOrFile = do
  isFile <- liftIO (doesFileExist dirOrFile)
  case (isFile, takeExtension dirOrFile) of
    (True,".nix") -> yield dirOrFile
    (True,_)     -> return ()
    (False,_)    -> liftIO (readDirectory dirOrFile) >>= mapM_ (withNixFiles yield . (dirOrFile </>))

