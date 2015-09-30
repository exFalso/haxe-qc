module TestCases where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.Monoid
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

data TestCase
  = TestCase
    { testCaseName   :: String
    , generateInput  :: Gen String
    , haxeExpression :: String
    , comparator     :: String -> String -> Bool
    }

readParse :: Read a => String -> Maybe a
readParse input = case reads input of
  [(a, [])] -> Just a
  _ -> Nothing

printableChar :: Gen Char
printableChar = do
  c <- arbitrary
  if isPrint c
    then return c
    else printableChar

testCases :: [TestCase]
testCases =
  [ TestCase
    { testCaseName = "StringToolsUrlDecode"
    , generateInput = listOf (frequency [(9, printableChar), (1, pure '%')])
    , haxeExpression = "function(s){return (try { StringTools.urlDecode(s); } catch(e:Dynamic) { \"<exception>\" + s; });}"
    , comparator = (==)
    }
  , TestCase
    { testCaseName = "StringToolsUrlEncode"
    , generateInput = listOf printableChar
    , haxeExpression = "function(s){return (try { StringTools.urlEncode(s); } catch(e:Dynamic) { \"<exception>\" + s; });}"
    , comparator = (==)
    }
  , TestCase
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
          , return "\b0"
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
