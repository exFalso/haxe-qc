{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

import Control.Exception
import Control.Concurrent
import Control.Monad
import Data.Monoid
import System.Directory
import System.FilePath
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.IO
import System.Exit

_BUILD :: FilePath
_BUILD = "./build"

_BUILD_HAXE :: FilePath
_BUILD_HAXE = _BUILD </> "haxe"

_BUILD_JS :: FilePath
_BUILD_JS = _BUILD </> "js"

_BUILD_JAVA :: FilePath
_BUILD_JAVA = _BUILD </> "java"

_SRC :: FilePath
_SRC = "./src"

_SRC_HAXE :: FilePath
_SRC_HAXE = _SRC </> "main" </> "haxe"

_SRC_HAXE_JAVA :: FilePath
_SRC_HAXE_JAVA = _SRC </> "main" </> "haxe_java"

data Backend
  = Backend
    { backendName :: String
    , compileShell :: FilePath -> String -> String
    , runShell :: String -> String
    }

backendJs :: Backend
backendJs =
  Backend
    { backendName = "js"
    , compileShell = \cp modul ->
        "haxe -js " <> (_BUILD_JS </> modul <> ".js") <>
        " -cp " <> _SRC_HAXE <>
        " -cp " <> cp <>
        " -cp /usr/lib/haxe/lib/nodejs/2,2,5" <>
        " -main " <> modul
    , runShell = \modul ->
        "node " <> (_BUILD_JS </> modul <> ".js")
    }
backendJava :: Backend
backendJava =
  Backend
    { backendName = "java"
    , compileShell = \cp modul ->
        "haxe -java " <> (_BUILD_JAVA </> modul <> ".java") <>
        " -cp " <> _SRC_HAXE <>
        " -cp " <> cp <>
        " -cp " <> _SRC_HAXE_JAVA <>
        " -main " <> modul
    , runShell = \modul -> "java -jar " <> (_BUILD_JAVA </> modul <> ".java" </> modul <> ".jar")
    }

data TestCase
  = TestCase
    { testCaseName :: String
    , generateInput :: Gen String
    , haxeExpression :: String
    , comparator :: String -> String -> Bool
    }

readParse :: Read a => String -> Maybe a
readParse input = case reads input of
  [(a, [])] -> Just a
  _ -> Nothing

testCases :: [TestCase]
testCases =
  [ TestCase
    { testCaseName = "ParseFloat"
    , generateInput =
        oneof
          [ do
               double <- arbitrary :: Gen Double
               return $ show double
          , arbitrary
          ]
    , haxeExpression = "function(s){return (\"\" + Std.parseFloat(s));}"
    , comparator =
        \astr bstr ->
          let
            doubleCmp :: Double -> Double -> Bool
            doubleCmp a b = a == b || isNaN a && isNaN b
          in case (readParse astr, readParse bstr) of
            (Just a, Just b) -> doubleCmp a b
            (a, b) -> a == b
    }
  ]

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
        putStrLn input
        putStrLn $ backendName etalon <> ":"
        putStrLn etalonOutput
        putStrLn $ backendName backend <> ":"
        putStrLn backendOutput
        fail ""

createDirs :: IO ()
createDirs = forM_ [ _BUILD_HAXE, _BUILD_JS, _BUILD_JAVA ] $ createDirectoryIfMissing True

main :: IO ()
main = do
  let
    etalon = backendJs
    backends = [backendJava]
  createDirs
  forM_ testCases $ \testCase -> do
    -- compile
    compileTestCase (etalon : backends) testCase
    -- test
    quickCheck $ qcTestCase etalon backends testCase
