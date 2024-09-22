-- 
-- This script was written by Alexey Khudyakov @shimuuar
-- during his work as Sirius.Courses
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
module Main(main) where
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.DeepSeq
import Development.Shake
import Development.Shake.FilePath
import Data.Aeson (Value(..),FromJSON(..),(.:),(.:?),(.!=),withObject)
import Data.Ord
import Data.Maybe
import Data.Functor
import Data.List (sortOn,isPrefixOf,isSuffixOf)
import Data.List.Split (splitWhen)
import Data.Yaml.Config qualified as YAML
import Data.Foldable
import Data.Hashable (Hashable(..))
import Data.Binary (Binary)
import Data.Version
import Data.Map.Strict qualified as Map
import PyF (fmt)
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Directory qualified as Dir (getDirectoryContents)
import GHC.Generics (Generic)


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Key to lookup source for given package.
newtype PkgName = PkgName String
  deriving stock   (Show, Eq, Generic)
  deriving newtype (Hashable, Binary, NFData)

type instance RuleResult PkgName = Package

-- | Key to lookup repository information
newtype Repository = Repository String
  deriving stock   (Show, Eq, Generic)
  deriving newtype (Hashable, Binary, FromJSON, NFData)

type instance RuleResult Repository = Git

-- | Key to obtain hackage revision
data ConfigRevisionKey = ConfigRevisionKey
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, FromJSON, NFData)

type instance RuleResult ConfigRevisionKey = String

-- | Key to obtain GHC version
data ConfigGhcVersion = ConfigGhcVersion
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, FromJSON, NFData)

type instance RuleResult ConfigGhcVersion = String

-- | Key to obtain GHC version
data ConfigHaddock = ConfigHaddock
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, FromJSON, NFData)

type instance RuleResult ConfigHaddock = Bool

-- | Key to obtain GHC version
data ConfigProfile = ConfigProfile
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, FromJSON, NFData)

type instance RuleResult ConfigProfile = Bool

-- | Key to obtain GHC version
data ConfigTests = ConfigTests
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, FromJSON, NFData)

type instance RuleResult ConfigTests = Bool

-- | Key to obtain jailbreak
data ConfigJailbreak = ConfigJailbreak
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, FromJSON, NFData)

type instance RuleResult ConfigJailbreak = Bool


data Config = Config
  { cfgRevision   :: !String -- ^ Hackage revision
  , cfgGhcVersion :: !String -- ^ GHC version to pass to cabal2nix
  , cfgProfile    :: !Bool   -- ^ Whether to build profiling
  , cfgHaddock    :: !Bool   -- ^ Whether to build haddocks
  , cfgTests      :: !Bool   -- ^ Whether to enable tests
  , cfgJailbreak  :: !Bool   -- ^ Whether to apply jailbreak to package
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    cfgRevision   <- o .: "revision"
    cfgGhcVersion <- o .: "ghc_version"
    cfgProfile    <- o .:? "profile"   .!= False
    cfgHaddock    <- o .:? "haddock"   .!= False
    cfgTests      <- o .:? "tests"     .!= False
    cfgJailbreak  <- o .:? "jailbreak" .!= True
    pure Config{..}


-- | Information about package.
data Package = Package
  { packageSource    :: Source   -- ^ Source location for package
  , packageParams    :: [String] -- ^ Code fragments to pass to package
  , packageJailbreak :: !Bool    -- ^ Whether to apply jailbreak to package
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Location of source code for package
data Source
  = SourceCabal Version                   -- ^ Fetch package from hackage
  | SourceGit   Git        (Maybe String) -- ^ Fetch package from git
  | SourceRef   Repository (Maybe String) -- ^ Use git repository referenced by name
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Information about git repository
data Git = Git
  { gitURL :: String -- ^ URI of git repository
  , gitRev :: String -- ^ Revision to fetch
  }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (Hashable, Binary, NFData)

instance FromJSON Package where
  parseJSON v@String{}   = do src <- parseJSON v
                              pure $ Package src [] True
  parseJSON v@(Object o) = do src   <- (SourceCabal <$> o .: "hackage") <|> parseJSON v
                              param <- o .:? "parameters" .!= []
                              jail  <- o .:? "jailbreak"  .!= True
                              pure $ Package src param jail
  parseJSON _ = fail "Cannot parse package"

instance FromJSON Source where
  parseJSON v@String{}   = SourceCabal <$> parseJSON v
  parseJSON v@(Object o) = asum
    [ SourceGit <$> parseJSON v   <*> (o .:? "subpath")
    , SourceRef <$> (o .: "repo") <*> (o .:? "subpath")
    ]
  parseJSON _ = fail "Cannot parse package source"

instance FromJSON Git where
  parseJSON = withObject "Git" $ \o -> do
    gitURL <- o .: "git"
    gitRev <- o .: "rev"
    pure Git{..}

-- | Newtype wrapper which accepts NULL
newtype OrNull a = OrNull (Map.Map String a)

instance FromJSON a => FromJSON (OrNull a) where
  parseJSON Null = pure $ OrNull mempty
  parseJSON o    = OrNull <$> parseJSON o


main :: IO ()
main = do
  shakeArgs shakeOptions $ do
    -- We read list of files for
    (pkgs_set, repo_set, config) <- do
      files <- liftIO $ Dir.getDirectoryContents "."
      let yamlWithPrefix pfx = sortOn Down [ path | path <- files
                                                  , pfx     `isPrefixOf` path
                                                  , ".yaml" `isSuffixOf` path
                                                  ]
          loadYaml :: FromJSON a => FilePath -> Rules a
          loadYaml pfx = liftIO $ YAML.loadYamlSettings (yamlWithPrefix pfx) [] YAML.ignoreEnv
      -- Read list of packages to build and create necessary oracles
      OrNull pkgs_set :: OrNull Package <- loadYaml "packages"
      OrNull repo_set :: OrNull Git     <- loadYaml "repo"
      config          :: Config         <- loadYaml "config"
      pure (pkgs_set, repo_set, config)
    get_source <- addOracle $ \(PkgName nm) -> do
      case nm `Map.lookup` pkgs_set of
        Just s  -> pure s
        Nothing -> error $ "No such package: " ++ nm
    get_git <- addOracle $ \(Repository nm) -> do
      case nm `Map.lookup` repo_set of
        Just s  -> pure s
        Nothing -> error $ "No such repository: " ++ nm
    get_revision  <- addOracle $ \ConfigRevisionKey -> pure $ cfgRevision   config
    get_ghcver    <- addOracle $ \ConfigGhcVersion  -> pure $ cfgGhcVersion config
    get_profile   <- addOracle $ \ConfigProfile     -> pure $ cfgProfile    config
    get_haddock   <- addOracle $ \ConfigHaddock     -> pure $ cfgHaddock    config
    get_tests     <- addOracle $ \ConfigTests       -> pure $ cfgTests      config
    get_jailbreak <- addOracle $ \ConfigJailbreak   -> pure $ cfgJailbreak  config
    -- Phony targets
    phony "clean" $ do
      removeFilesAfter "nix/" ["pkgs/haskell/*.nix", "default.nix"]
      removeFilesAfter "."   [".shake"]
    phony "list-new" $ listNewPackages pkgs_set
    -- Show diff for package in set and latest version
    forM_ [(k,v) | (k, SourceCabal v) <- Map.toList (packageSource <$> pkgs_set)] $ \(pkg, v) -> do
      phony ("diff@"<>pkg) $ do
        liftIO $ do putStrLn pkg
                    print v
        withTempDir $ \dir_latest ->
          withTempDir $ \dir_current -> do
            command_ [] "cabal" [ "unpack"
                                , pkg
                                , "-d", dir_latest]
            command_ [] "cabal" [ "unpack"
                                , [fmt|{pkg}-{showVersion v}|]
                                , "-d", dir_current
                                ]
            [latest]  <- getDirectoryContents dir_latest
            [current] <- getDirectoryContents dir_current
            Exit _ <- command [] "colordiff" ["-u", "-r", "-Z"
                                             , dir_current</>current
                                             , dir_latest</>latest
                                             ]
            return ()
    -- Generate files for each package
    for_ (Map.keys pkgs_set) $ \pkg -> do
      let fname = "nix" </> packageNixName pkg
      fname %%> \_ -> do
        let patch_name = "./patches" </> pkg <.> "nix" <.> "patch"
        exists <- doesFileExist patch_name
        when exists $ need [patch_name]
        let andPatch = when exists $ command_ [FileStdin patch_name] "patch" [fname]
        ghc <- get_ghcver ConfigGhcVersion
        (get_source (PkgName pkg) <&> packageSource) >>= \case
          SourceCabal v           -> do
            rev <- get_revision ConfigRevisionKey
            cabal2nixHackage fname pkg v rev ghc
            andPatch
          SourceGit git  msubpath -> do
            cabal2nixGit fname git ghc msubpath
            andPatch
          SourceRef repo msubpath -> do
            git <- get_git repo
            cabal2nixGit fname git ghc msubpath >> andPatch
    -- Building nix overlay
    "nix/default.nix" %%> \overlay -> do
      need $ (\x -> "nix" </> packageNixName x) <$> Map.keys pkgs_set
      need ["packages.yaml", "repo.yaml"]
      haddock::String <- get_haddock ConfigHaddock <&> \case
        True  -> "lib.doHaddock"
        False -> "lib.dontHaddock"
      tests::String <- get_tests ConfigTests <&> \case
        True  -> "lib.doCheck"
        False -> "lib.dontCheck"
      profile::String <- get_profile ConfigProfile <&> \case
        True  -> "lib.enableLibraryProfiling"
        False -> "lib.disableLibraryProfiling"
      jailbreak::String <- get_jailbreak ConfigJailbreak <&> \case
        True  -> "lib.doJailbreak"
        False -> "lib.dontJailbreak"
      liftIO $ writeFile overlay $ unlines $ concat
        [ [ "pkgs: prev:"
          , "let"
          , "  lib = pkgs.haskell.lib;"
          , [fmt|  adjust = drv: {jailbreak} ({profile} ({haddock} ({tests} drv)));|]
          , "in"
          , "{"
          ]
        , [ [fmt|  {nm} = {fin};|]
          | (nm, Package{packageParams=param,packageJailbreak=jail}) <- Map.toList pkgs_set
          , let pkg,fin :: String
                pkg = [fmt|adjust (prev.callPackage ./{packageNixName nm} {{ {concat $ fmap (++";") param} }})|]
                fin | jail      = [fmt|lib.doJailbreak ({pkg})|]
                    | otherwise = pkg
          ]
        , ["}"]
        ]
    -- Default action
    want $ (\x -> "nix" </> packageNixName x) <$> Map.keys pkgs_set
    want ["nix/default.nix"]

cabal2nixHackage :: FilePath -> String -> Version -> String -> String -> Action ()
cabal2nixHackage fname pkg v rev ghc = command_ [FileStdout fname] "cabal2nix" $
  [ [fmt|cabal://{pkg}-{showVersion v}|]
  , "--hackage-snapshot", rev
  , "--compiler",         ghc
  ]

cabal2nixGit :: FilePath -> Git -> String -> Maybe String -> Action ()
cabal2nixGit fname Git{..} ghc msubpath = command_ [FileStdout fname] "cabal2nix" $
  [ gitURL
  , "--revision", gitRev
  , "--compiler", ghc
  ] ++
  case msubpath of
    Nothing -> []
    Just s  -> ["--subpath", s]


----------------------------------------------------------------
-- List package which we fetch from hackage and which are older than
-- latest version
----------------------------------------------------------------

listNewPackages :: Map.Map String Package -> Action ()
listNewPackages pkgs = do
  StdoutTrim str <- command [] "bash" ["-c", "tar tf ~/.cabal/packages/hackage.haskell.org/01-index.tar.gz"]
  let hackage_ver = Map.fromListWith max $ mapMaybe parseIndexLine $ lines str
      local_ver   = Map.mapMaybe (\x -> do { SourceCabal v <- Just (packageSource x); pure v }) pkgs
      -- Select only versions that are newer on hackage
      (patch_new, newer) = Map.partition (uncurry onlyPatchVersionDiff)
                         $ Map.filter    (uncurry (<))
                         $ Map.intersectionWith (,) local_ver hackage_ver
  liftIO $ putStrLn "== Patch upgrades =="
  liftIO $ mapM_ reportVersionDifference $ Map.toList patch_new
  liftIO $ putStrLn "== Upgrades =="
  liftIO $ mapM_ reportVersionDifference $ Map.toList newer

reportVersionDifference :: (String, (Version,Version)) -> IO ()
reportVersionDifference (nm, (v1,v2)) = putStrLn [fmt|{nm:30s} {showVersion v1} -> {showVersion v2}|]

onlyPatchVersionDiff :: Version -> Version -> Bool
onlyPatchVersionDiff (Version (smaj1:maj1:min1:_) _) (Version (smaj2:maj2:min2:_) _)
  = smaj1 == smaj2 && maj1 == maj2 && min1 == min2
onlyPatchVersionDiff _ _ = False

-- Parse file name from index.tar.gz file
parseIndexLine :: String -> Maybe (String,Version)
parseIndexLine str = case splitWhen (=='/') str of
  [_,"preferred-versions"] -> Nothing
  [nm,v,_] -> Just (nm, parseV v)
  _        -> failed
  where
    failed :: a
    failed = error $ "Cannot parse cabal file name: " ++ str
    parseV s = case [ v | (v,"") <- readP_to_S parseVersion s ] of
      [v] -> v
      _   -> failed


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

packageNixName :: String -> FilePath
packageNixName nm = "pkgs" </> "haskell" </> nm <.> "nix"

-- Same as  %> but removes target in case of exception
(%%>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
pat %%> callback = pat %> \nm -> callback nm `actionOnException` removeFiles "." [nm]
