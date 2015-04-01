{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Monoid
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic

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
        " -lib nodejs" <>
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
    { testCaseName = "StdParseInt"
    , generateInput =
        oneof
          [ do
               double <- arbitrary :: Gen Integer
               return $ show double
          , arbitrary
          , do
               double <- arbitrary :: Gen Integer
               str <- arbitrary
               return $ show double <> str
          , ("0x" <>) <$> arbitrary
          ]
    , haxeExpression = "function(s){return (\"\" + Std.parseInt(s));}"
    , comparator = \astr bstr -> (readParse astr :: Maybe Integer) == readParse bstr
    }
  , TestCase
    { testCaseName = "StdParseFloat"
    , generateInput =
        oneof
          [ do
               double <- arbitrary :: Gen Double
               return $ show double
          , arbitrary
          , do
               double <- arbitrary :: Gen Double
               str <- arbitrary
               return $ show double <> str
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
  , TestCase
    { testCaseName = "StdInt"
    , generateInput = do
        double <- arbitrary :: Gen Double
        return $ show double
    , haxeExpression = "function(s){return (\"\" + Std.int(Std.parseFloat(s)));}"
    , comparator = \astr bstr -> (readParse astr :: Maybe Integer) == readParse bstr
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
        print input
        putStrLn $ backendName etalon <> ":"
        print etalonOutput
        putStrLn $ backendName backend <> ":"
        print backendOutput
        fail ""

createDirs :: IO ()
createDirs = forM_ [ _BUILD_HAXE, _BUILD_JS, _BUILD_JAVA ] $ createDirectoryIfMissing True

forkWait :: IO () -> IO (IO ())
forkWait action = do
  v <- newEmptyMVar
  _ <- forkIO $ action >> putMVar v ()
  return (readMVar v)

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
    waits <- replicateM 5 . forkWait $
      quickCheckWith stdArgs { maxSuccess = 10000, chatty = False } $
        qcTestCase etalon backends testCase
    sequence_ waits
