{-# LANGUAGE LambdaCase #-}

-- | For each "test_*" directory that is not listed in the
-- "DISABLED_TESTS" environment variable or the disabled_tests.txt if
-- the environment variable isn't set, invoke the test.sh in that
-- directory with some supporting environment variables set.  The
-- test.sh should complete with a return code of 0 on no error or
-- non-zero on error.
--
-- If the DISABLED_TESTS environment variable is set, the
-- disabled_tests.txt file is ignored.  Both may specify tests
-- separated by spaces and/or newlines, and the '#' character starts a
-- comment to the end of the current line.
--
-- The ENABLED_TESTS environment variable, if set, overrides the set
-- of discovered tests to include only those in the ENABLED_TESTS
-- list.  This environment variable is commonly used during
-- development to run specific tests (which `cabal test` does not
-- easily support).

module Main where

import Control.Monad ( filterM, foldM, join, unless )
import Control.Monad.IO.Class (liftIO)
import Data.List ( isPrefixOf, intercalate, sort )
import Data.Maybe ( fromMaybe )
import System.Directory ( getCurrentDirectory, findExecutable, listDirectory
                        , doesDirectoryExist, doesFileExist )
import System.Environment ( lookupEnv )
import System.Exit ( ExitCode(ExitSuccess), exitFailure )
import System.FilePath ( (</>), pathSeparator, searchPathSeparator
                       , takeDirectory, takeFileName, isAbsolute )
import System.FilePath.Find ( always, find, extension, (==?) )
import System.IO ( hPutStrLn, stderr )
import System.Process ( readProcessWithExitCode
                      , readCreateProcessWithExitCode
                      , shell, CreateProcess(..) )
-- import qualified Paths_saw_script as P
import Test.Tasty
    ( defaultMain, localOption, testGroup, mkTimeout, TestTree )
import Test.Tasty.HUnit ( testCase, (@=?), assertBool )
import Test.Tasty.ExpectedFailure ( ignoreTest )


-- | Reads from DISABLED_TESTS env var or disabled_tests.txt file
-- (preference is given to the former) and returns the list of tests.
-- Shell-style comments are removed, and test names are assumed to be
-- a single word without whitespace.  The input can separate testnames
-- with any type of whitespace (space, tab, newline).
--
-- Returns the list of disabled test names.
getDisabledTestList :: FilePath -> IO [String]
getDisabledTestList testdir = do
  let dtfile = testdir </> "disabled_tests.txt"
  dset <- lookupEnv "DISABLED_TESTS" >>= \case
    Just d -> return d
    Nothing -> readFile dtfile
  let removeComment = takeWhile ((/=) '#')
      stripWhitespace = words
      processInput = join . map (stripWhitespace . removeComment) . lines
  return $ processInput dset


-- | Gets the list of tests (subdirectories) to run given the base
-- directory and the list of disabled tests
getTestList :: FilePath -> [String] -> IO [String]
getTestList testdir disabled = do
  f <- listDirectory testdir
  let isTest = filter (isPrefixOf "test")
      notDisabled = filter (not . flip elem disabled)
      inTestdir = fmap (testdir </>)
  filterM doesDirectoryExist $ inTestdir $ notDisabled $ isTest $ sort f


-- ----------------------------------------------------------------------=
-- * Environment variable handling.
--
-- Start with an initial set of variables and an asociated value (or
-- set of values with a separator), then override/update with any
-- environment variables set.

data EnvVarSpec = EV String String
                  -- ^ single string value
                | EVp String Char [String]
                  -- ^ accumulative path with separator

updEnvVars :: String -> String -> [EnvVarSpec] -> [EnvVarSpec]
updEnvVars n v [] = [EV n v | v /= ""]
updEnvVars n v (EV  n'   v' : evs) | n == n' = EV  n (if v == "" then v' else v) : evs
updEnvVars n v (EVp n' s v' : evs) | n == n' = EVp n s (v' <> [v]) : evs
updEnvVars n v (ev : evs) = ev : updEnvVars n v evs

envVarAssocList :: [EnvVarSpec] -> [(String, String)]
envVarAssocList = map envVarAssoc
  where
    envVarAssoc (EV  n v)    = (n, v)
    envVarAssoc (EVp n s vs) = (n, intercalate [s] vs)

-- ----------------------------------------------------------------------
-- * Test Parameters
--
--  Determine all Environment Variables that should be set for the
--  tests, using the internal defaults plus overrides from the current
--  environment:
--     - SAW = saw invocation command (with jars specification)
--     - JAVA = path to java runtime
--     - HOME = directory to act as home when running tests
--     - PATH = PATH searchlist, supplemented with discovered elements
--     - JAVA_HOME = path to java installation
--     - TESTBASE = path to intTests directory
--     - SAW_JDK_JAR = path to rt.jar
--
--  These environment variables may already be set to supply default
--  locations for these components.
--
--  Also determine the list of JAR files to pass to the various tests:
--     - The rt.jar runtime library from the JAVA installation
--     - Any jars supplied with the jvm-verifier
--     - Any jars supplied by saw
--
-- Note that even if SAW is specified in the environment, this test runner will
-- augment those definitions with the discovered jar files and target path
-- specifications.


-- | Returns the environment variable assocList to use for running
-- each individual test
testParams :: FilePath -> (String -> IO ()) -> IO [(String, String)]
testParams intTestBase verbose = do
  here <- getCurrentDirectory
  let absTestBase = if isAbsolute intTestBase then intTestBase
                    else here </> intTestBase

  -- try to determine where the saw binary is in case there are other
  -- executables there (e.g. z3, etc.)
  sawExe <- let v1loc = here </> "dist" </> "build" </> "saw" </> "saw"
            in doesFileExist v1loc >>= \case
                 True -> return v1loc
                 False -> readProcessWithExitCode "cabal" ["v2-exec", "which", "saw"] "" >>= \case
                   (ExitSuccess, o, _) -> return o
                   _ -> lookupEnv "SAW" >>= \case
                     Just s -> if isAbsolute s then return s else return (here </> s)
                     Nothing -> findExecutable "saw" >>= \case
                       Just e -> return e
                       _ -> return "" -- may be supplied via env var
  verbose $ "Locally-built saw exe at: " <> sawExe
  let eVars0 = [ EV  "HOME"     absTestBase
               , EVp "PATH"     searchPathSeparator [takeDirectory sawExe]
               , EV  "TESTBASE" absTestBase
               , EV  "DIRSEP"   [pathSeparator]
               , EV  "CPSEP"    [searchPathSeparator]

               -- The eval is used to protect the evaluation of the
               -- single-quoted arguments supplied below when run in a
               -- bash test.sh script.
               , EVp "SAW"      ' ' ["eval", "saw"]
               ]
      addEnvVar evs e = do v <- lookupEnv e
                           return $ updEnvVars e (fromMaybe "" v) evs
  -- override eVars0 with any environment variables set in this process
  e1 <- foldM addEnvVar eVars0 [ "SAW", "PATH", "JAVAC", "JAVA_HOME", "SAW_JDK_JAR" ]

  -- Create a pathlist of jars for invoking saw
  let jarsDir = absTestBase </> "jars"

  let findJarsIn p = doesDirectoryExist p >>= \case
        True -> find always (extension ==? ".jar") p
        False -> return []

  verbose $ "Finding JARs in " <> jarsDir
  jars <- intercalate [searchPathSeparator] <$> findJarsIn jarsDir

  -- Set the SAW env var for the testing scripts to invoke saw with the JAR
  -- list, again allowing ENV override.
  let e3 = updEnvVars "SAW" (unwords [ "-j", "'" <> jars <> "'" ]) e1

  return $ envVarAssocList e3


-- ----------------------------------------------------------------------

-- | Generate a HUnit test for each test, executed by running the
-- test.sh file in that test directory.  The first argument is the set
-- of environment variables to set when running the tests and the
-- second is the list of disabled tests.
genTests :: [(String,String)] -> [String] -> [String] -> [TestTree]
genTests envvars disabled = map mkTest
  where
    preTest n = if takeFileName n `elem` disabled then ignoreTest else id
    mkTest n = preTest n $ testCase (takeFileName n) $ do
      let cmd = (shell "bash test.sh") { cwd = Just n, env = Just envvars }
      (r,o,e) <- liftIO $ readCreateProcessWithExitCode cmd ""
      if r == ExitSuccess
        then return ()
        else putStrLn o >> hPutStrLn stderr e
      r @=? ExitSuccess


-- | Several of the tests use definitions from the cryptol-specs
-- repository, which should be present in deps/cryptol-specs.
-- Verify the existence of that repository if any of the tools here
-- need it.
check_cryptol_specs :: String -> [String] -> [String] -> TestTree
check_cryptol_specs testPath disabled tests = testCase "cryptol-specs Available" $
  let need_cryptol_spec = any (\t -> let tp = (testPath </> t)
                                     in tp `elem` tests && not (t `elem` disabled))
                          [ "test0001", "test0002"
                          , "test0006", "test0006_w4"
                          , "test0035_aes_consistent"
                          , "test_w4"
                          , "test_examples"
                          ]
      cspec_dir = takeDirectory testPath </> "deps" </> "cryptol-specs"
  in if need_cryptol_spec
     then do have_cs <- liftIO $ doesDirectoryExist $ cspec_dir </> "Primitive"
             unless (have_cs) $ liftIO $ do
               hPutStrLn stderr "Tests require cryptol-specs as a checked-out subrepo:"
               hPutStrLn stderr "  $ git submodule update --init deps/cryptol-specs"
             assertBool "Missing cryptol-specs" have_cs
     else return ()


main :: IO ()
main = do
  let base = "intTests"
  -- Run tests with VERBOSE=y environment variable for extra output.
  verbose <- lookupEnv "VERBOSE" >>= \case
    Just "y" -> return $ putStrLn
    _ -> return $ const (return ())
  found <- doesDirectoryExist base
  unless found $ do
    curwd <- getCurrentDirectory
    hPutStrLn stderr $ "FAILURE: cannot find test directory " <> base <> " from " <> curwd
    exitFailure
  dset <- getDisabledTestList base
  verbose $ "Disabled tests: " <> show dset
  testdirs' <- getTestList base [] -- no filtering here; they will be ignoreTest'd by genTests
  testdirs <- fromMaybe testdirs' .
              (fmap (\et -> let path_ets = fmap (base </>) $ words et in
                              filter (flip elem path_ets) testdirs')) <$>
              lookupEnv "ENABLED_TESTS"
  envVars <- testParams base verbose
  verbose $ "ENV: " <> show envVars
  defaultMain $
    localOption (mkTimeout $ 5 * 60 * 1000 * 1000) $  -- 5 minute timeout in usecs
    testGroup "intTests" $
    check_cryptol_specs base dset testdirs : (genTests envVars dset) testdirs
