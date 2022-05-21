{-# LANGUAGE CPP #-}

-- | Automatic test discovery and runner for the tasty framework.
module Test.Tasty.Discover
  ( -- * Main Test Generator
    generateTestDriver

    -- * For Testing Purposes Only
  , ModuleTree (..)
  , findTests
  , mkModuleTree
  , showTests
  ) where

import Data.List            (dropWhileEnd, intercalate, isPrefixOf, nub, sort, stripPrefix)
import Data.Maybe           (fromMaybe)
import System.FilePath      (pathSeparator)
import System.FilePath.Glob (compile, globDir1, match)
import System.IO            (IOMode (ReadMode), withFile)
import Test.Tasty.Config    (Config (..), GlobPattern)
import Test.Tasty.Generator (Generator (..), Test (..), generators, getGenerators, mkTest, showSetup)

import qualified Data.Map.Strict as M

#if defined(mingw32_HOST_OS)
import GHC.IO.Encoding.CodePage (mkLocaleEncoding)
import GHC.IO.Encoding.Failure  (CodingFailureMode (TransliterateCodingFailure))
import GHC.IO.Handle            (hGetContents, hSetEncoding)
#else
import GHC.IO.Handle (hGetContents)
#endif

-- | Main function generator, along with all the boilerplate which
-- which will run the discovered tests.
generateTestDriver :: Config -> String -> [String] -> FilePath -> [Test] -> String
generateTestDriver config modname is src tests =
  let generators' = getGenerators tests
      testNumVars = map (("t"++) . show) [(0 :: Int)..]
  in concat
    [ "{-# LANGUAGE FlexibleInstances #-}\n"
    , "\n"
    , "module " ++ modname ++ " (main, ingredients, tests) where\n"
    , "\n"
    , "import Prelude\n"
    , "\n"
    , "import qualified System.Environment as E\n"
    , "import qualified Test.Tasty as T\n"
    , "import qualified Test.Tasty.Ingredients as T\n"
    , unlines $ map generatorImport generators'
    , showImports (map ingredientImport is ++ map testModule tests)
    , "{- HLINT ignore \"Use let\" -}\n"
    , "\n"
    , unlines $ map generatorClass generators'
    , "tests :: IO T.TestTree\n"
    , "tests = do\n"
    , unlines $ zipWith showSetup tests testNumVars
    , "  pure $ T.testGroup " ++ show src ++ " ["
    , intercalate "," $ showTests config tests testNumVars
    , "]\n"
    , "ingredients :: [T.Ingredient]\n"
    , "ingredients = " ++ ingredients is ++ "\n"
    , "main :: IO ()\n"
    , "main = do\n"
    , "  args <- E.getArgs\n"
    , "  E.withArgs (" ++ show (tastyOptions config) ++ " ++ args) $"
    , "    tests >>= T.defaultMainWithIngredients ingredients\n"
    ]

-- | Match files by specified glob pattern.
filesByModuleGlob :: FilePath -> Maybe GlobPattern -> IO [String]
filesByModuleGlob directory globPattern = globDir1 pattern directory
  where pattern = compile ("**/" ++ fromMaybe "*.hs*" globPattern)

-- | Filter and remove files by specified glob pattern.
ignoreByModuleGlob :: [FilePath] -> Maybe GlobPattern -> [FilePath]
ignoreByModuleGlob filePaths Nothing = filePaths
ignoreByModuleGlob filePaths (Just ignoreGlob) = filter (not . match pattern) filePaths
  where pattern = compile ("**/" ++ ignoreGlob)

-- | Discover the tests modules.
findTests :: Config -> IO [Test]
findTests config = do
  let directory = searchDir config
  allModules <- filesByModuleGlob directory (modules config)
  let filtered = ignoreByModuleGlob allModules (ignores config)
      -- The files to scan need to be sorted or otherwise the output of
      -- findTests might not be deterministic
      sortedFiltered = sort filtered
  concat <$> traverse (extract directory) sortedFiltered
  where extract directory filePath =
          withFile filePath ReadMode $ \h -> do
#if defined(mingw32_HOST_OS)
          -- Avoid internal error: hGetContents: invalid argument (invalid byte sequence)' non UTF-8 Windows
            hSetEncoding h $ mkLocaleEncoding TransliterateCodingFailure
#endif
            tests <- extractTests (dropDirectory directory filePath) <$> hGetContents h
            seq (length tests) (return tests)
        dropDirectory directory filePath = fromMaybe filePath $
          stripPrefix (directory ++ [pathSeparator]) filePath

-- | Extract the test names from discovered modules.
extractTests :: FilePath -> String -> [Test]
extractTests file = mkTestDeDuped . isKnownPrefix . parseTest
  where mkTestDeDuped = map (mkTest file) . nub
        isKnownPrefix = filter (\g -> any (checkPrefix g) generators)
        checkPrefix g = (`isPrefixOf` g) . generatorPrefix
        parseTest     = map fst . concatMap lex . lines

-- | Show the imports.
showImports :: [String] -> String
showImports mods = unlines $ nub $ map (\m -> "import qualified " ++ m ++ "\n") mods

-- | Retrieve the ingredient name.
ingredientImport :: String -> String
ingredientImport = init . dropWhileEnd (/= '.')

-- | Ingredients to be included.
ingredients :: [String] -> String
ingredients is = concat $ map (++":") is ++ ["T.defaultIngredients"]

-- | Show the tests.
showTests :: Config -> [Test] -> [String] -> [String]
showTests config tests testNumVars = if treeDisplay config
  then showModuleTree $ mkModuleTree tests testNumVars
  else zipWith (curry snd) tests testNumVars

newtype ModuleTree = ModuleTree (M.Map String (ModuleTree, [String]))
  deriving (Eq, Show)

showModuleTree :: ModuleTree -> [String]
showModuleTree (ModuleTree mdls) = map showModule $ M.assocs mdls
  where -- special case, collapse to mdl.submdl
        showModule (mdl, (ModuleTree subMdls, [])) | M.size subMdls == 1 =
          let [(subMdl, (subSubTree, testVars))] = M.assocs subMdls
          in showModule (mdl ++ '.' : subMdl, (subSubTree, testVars))
        showModule (mdl, (subTree, testVars)) = concat
          [ "T.testGroup \"", mdl
          , "\" [", intercalate "," (showModuleTree subTree ++ testVars), "]" ]

mkModuleTree :: [Test] -> [String] -> ModuleTree
mkModuleTree tests testVars = ModuleTree $
    foldr go M.empty $ zipWith (\t tVar -> (testModule t, tVar)) tests testVars
  where go (mdl, tVar) mdls = M.insertWith merge key val mdls
          where (key, val) = case break (== '.') mdl of
                  (_, [])              -> (mdl, (ModuleTree M.empty, [tVar]))
                  (topMdl, '.':subMdl) -> (topMdl, (ModuleTree $ go (subMdl, tVar) M.empty, []))
                  _                    -> error "impossible case in mkModuleTree.go.key"
        merge (ModuleTree mdls1, tVars1) (ModuleTree mdls2, tVars2) =
          (ModuleTree $ M.unionWith merge mdls1 mdls2, tVars1 ++ tVars2)
