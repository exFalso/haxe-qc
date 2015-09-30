{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           TestCases

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.Monoid
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

_BUILD :: FilePath
_BUILD = "./build"

_BUILD_HAXE :: FilePath
_BUILD_HAXE = _BUILD </> "haxe"

_BUILD_JS :: FilePath
_BUILD_JS = _BUILD </> "js"

_BUILD_JAVA :: FilePath
_BUILD_JAVA = _BUILD </> "java"

_BUILD_JAVA_COMMON :: FilePath
_BUILD_JAVA_COMMON = _BUILD_JAVA </> "common"

_SRC :: FilePath
_SRC = "./src"

_SRC_HAXE :: FilePath
_SRC_HAXE = _SRC </> "main" </> "haxe"

_SRC_JAVA :: FilePath
_SRC_JAVA = _SRC </> "main" </> "java"

_SRC_HAXE_JAVA :: FilePath
_SRC_HAXE_JAVA = _SRC </> "main" </> "haxe_java"

data Backend
  = Backend
    { backendName  :: String
    , compileShell :: FilePath -> String -> String
    , runShell     :: String -> String
    }

backendJs :: Backend
backendJs =
  Backend
    { backendName = "js"
    , compileShell = \cp modul ->
        "haxe -js " <> (_BUILD_JS </> modul <> ".js") <>
        " -cp " <> _SRC_HAXE <>
        " -cp " <> cp <>
        " -lib nodejs" <>
        " -main " <> modul
    , runShell = \modul ->
        "node " <> (_BUILD_JS </> modul <> ".js")
    }
backendJava :: Bool -> Backend
backendJava override =
  Backend
    { backendName = "java"
    , compileShell = \cp modul ->
            "haxe -java " <> (_BUILD_JAVA </> modul <> ".java") <>
            " -cp " <> _SRC_HAXE <>
            " -cp " <> cp <>
            (if override then " -cp " <> _SRC_HAXE_JAVA else "") <>
            " -java-lib " <> _BUILD_JAVA_COMMON <>
            " -main " <> modul <>
            " -D no-compilation" <>
          " && " <>
            "mkdir -p " <> (_BUILD_JAVA </> modul <> ".java" </> "obj") <>
          " && " <>
            "javac -d " <> (_BUILD_JAVA </> modul <> ".java" </> "obj") <>
            " $(find " <> (_BUILD_JAVA </> modul <> ".java" </> "src") <> " -name \"*.java\")" <>
            " $(find " <> _SRC_JAVA <> " -name \"*.java\")" <>
          " && " <>
            "jar -cfe " <> (_BUILD_JAVA </> modul <> ".java" </> modul <> ".jar") <>
            " haxe.root." <> modul <>
            " -C build/java/" <> modul <> ".java/obj ."
    , runShell = \modul ->
        "java -jar " <> (_BUILD_JAVA </> modul <> ".java" </> modul <> ".jar") <>
        " -cp " <> _BUILD_JAVA_COMMON
    }

makeMain :: String -> String -> String
makeMain = \name fun -> "import InOut;\nclass " <> name <> "{ public static function main() { InOut.run(" <> fun <> ");} }"

compileTestCase :: [Backend] -> TestCase -> IO ()
compileTestCase backends TestCase{..} = do
  writeFile (_BUILD_HAXE </> testCaseName <> ".hx") $ makeMain testCaseName haxeExpression
  forM_ backends $ \Backend{..} -> callCommand (compileShell _BUILD_HAXE testCaseName)

runBackend :: Backend -> TestCase -> String -> IO String
runBackend Backend{..} TestCase{..} input = do
  exitCMVar <- newEmptyMVar
  (Just stin, Just stout, _, prHandle) <-
    createProcess (shell $ runShell testCaseName) { std_in = CreatePipe, std_out = CreatePipe }

  _ <- forkIO $ do
    e <- try $ do
      hPutStrLn stin input
      hFlush stin
      hClose stin
    case e of
      Left (ioe :: IOException) -> fail $ "Exception while writing to shell process: " <> show ioe
      _ -> return ()
  putMVar exitCMVar =<< waitForProcess prHandle
  output <- hGetContents stout
  exitC <- readMVar exitCMVar
  case exitC of
    ExitFailure e -> fail $ "Shell process returned with exit code " ++ show e ++ ", its stdout: " ++ show output
    _ -> return output

qcTestCase :: Backend -> [Backend] -> TestCase -> Property
qcTestCase etalon backends testCase@TestCase{..} = property $ do
  input <- generateInput
  return . monadicIO $ do
    etalonOutput <- run $ runBackend etalon testCase input
    forM_ backends $ \backend -> do
      backendOutput <- run $ runBackend backend testCase input
      unless (etalonOutput `comparator` backendOutput) . run $ do
        putStrLn $ "Difference between " <> backendName etalon <> " and " <> backendName backend <> " when checking " <> testCaseName
        putStrLn "input:"
        print input
        putStrLn $ backendName etalon <> ":"
        print etalonOutput
        putStrLn $ backendName backend <> ":"
        print backendOutput
        fail ""

createDirs :: IO ()
createDirs = forM_ [ _BUILD, _BUILD_HAXE, _BUILD_JS, _BUILD_JAVA, _BUILD_JAVA_COMMON ] $ createDirectoryIfMissing True

forkWait :: IO a -> IO (IO a)
forkWait action = do
  v <- newEmptyMVar
  _ <- forkIO $ action >>= putMVar v
  return (readMVar v)

main :: IO ()
main = do
  args <- getArgs
  let
    overrideJava = case args of
      ["--use-override"] -> True
      _ -> False
    etalon = backendJs
    backends = [backendJava overrideJava]
  createDirs
  results <- forM testCases $ \testCase -> do
    -- compile
    putStrLn $ "[" <> testCaseName testCase <> "] Compiling..."
    compileTestCase (etalon : backends) testCase
    -- test
    putStrLn $ "[" <> testCaseName testCase <> "] Running test..."
    waits <- replicateM 5 . forkWait $
      quickCheckWithResult stdArgs { maxSuccess = 1000 } $
        qcTestCase etalon backends testCase
    sequence waits
  forM_ (concat results) $ \res -> case res of
    Failure{} -> exitFailure
    _ -> return ()
