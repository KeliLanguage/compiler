{-# LANGUAGE BangPatterns #-}

import Test.Hspec
import Parser
import Debug.Trace
import System.Directory
import Control.Monad
import Data.Strings
import Data.List
import Data.String.Utils

import Util
import Interpreter
import Ast.Verified (newStringToken, newStringToken', StringToken)
import CompletionItems
import Debug.Pretty.Simple (pTraceShowId, pTraceShow)

testParseKeli :: String -> Expectation
testParseKeli x = 
    (case (keliParse "<test>" x) of
        Right _   -> True
        Left  err -> trace (show err) $ False) `shouldBe` True

runTestCases_compile :: IO ()
runTestCases_compile = do
    let parentDir = "./test/specs/execute/"
    allTestCases <- listDirectory parentDir

    -- search for test cases prefix with `ONLY:`
    let onlyTestCases = filter (\t -> t `strStartsWith` "ONLY:") allTestCases
    let finalTestCases = if length onlyTestCases > 0 then onlyTestCases else allTestCases
    hspec $ do
        forM_
            finalTestCases 
            (\t -> do
                describe "~" $ do
                    it t $ do
                        -- 1. validate if the test cases is structured in the correct format
                        validateTestCase parentDir t

                        -- 2. interpret the entry file of this test cases
                        result <- keliInterpret True (parentDir ++ t ++ "/entry.keli") 

                        -- 3. compare the output with expected output
                        expectedOutput <- readFile (parentDir ++ t ++ "/output")
                        case result of 
                            Right output ->
                                strip output `shouldBe` strip expectedOutput
                            Left err ->
                                strip err `shouldBe` strip expectedOutput)

    where

        -- This function is to make sure the following files exist
        -- * entry.keli
        -- * output
        validateTestCase :: String -> String -> IO ()
        validateTestCase parentDir testCaseName = do
            filenames <- listDirectory (parentDir ++ testCaseName)
            if ["entry.keli", "output"] `isSubListOf` filenames then
                return ()
            else
                error ("The file `entry.keli` and `output` should be created inside " ++ parentDir ++ testCaseName)


runTestCases_suggest :: IO ()
runTestCases_suggest = do
    let parentDir = "./test/specs/suggest/"
    allTestCases <- listDirectory parentDir

    -- search for test cases prefix with `ONLY:`
    let onlyTestCases = filter (\t -> t `strStartsWith` "ONLY:") allTestCases
    let finalTestCases = if length onlyTestCases > 0 then onlyTestCases else allTestCases
    hspec $ do
        forM_
            finalTestCases 
            (\t -> do
                describe "~" $ do
                    it t $ do
                        -- 1. validate if the test cases is structured in the correct format
                        validateTestCase parentDir t

                        -- 2. extract position
                        position <- readFile (parentDir ++ t ++ "/where")
                        let [lineNumber, columnNumber] = map (\x -> (read x) :: Int) (lines position)

                        -- 3. look for suggestion at the entry file of this test cases
                        output <- suggestCompletionItemsAt (parentDir ++ t ++ "/entry.keli") (lineNumber, columnNumber)

                        -- 4. compare the output with expected output
                        expectedOutput <- readFile (parentDir ++ t ++ "/output")
                        output `shouldBe` (read expectedOutput :: [CompletionItem]))

    where

        -- This function is to make sure the following files exist
        -- * entry.keli
        -- * output
        -- * where
        validateTestCase :: String -> String -> IO ()
        validateTestCase parentDir testCaseName = do
            filenames <- listDirectory (parentDir ++ testCaseName)
            if sort ["entry.keli", "output", "where"] `isSubListOf` sort filenames then
                return ()
            else
                error ("The file `entry.keli`, `where` and `output` should be created inside " ++ parentDir ++ testCaseName)

-- copied from https://stackoverflow.com/questions/47232335/check-if-list-is-a-sublist-of-another-list
isSubListOf :: Eq a => [a] -> [a] -> Bool
isSubListOf [] [] = True
isSubListOf _ []    = False
isSubListOf [] _    = True
isSubListOf (x:xs) (y:ys) 
    | x == y    = isSubListOf xs ys   
    | otherwise = isSubListOf (x:xs) ys


main :: IO ()
main = do 
    otherTest
    runTestCases_suggest
    runTestCases_compile

targetTags :: [StringToken]
targetTags = [newStringToken "a", newStringToken "b", newStringToken "c"]

otherTest :: IO ()
otherTest = hspec $ do
    describe "match" $ do
        it "got duplicate" $ do
            let source = [
                    newStringToken' (0,0,"a"),
                    newStringToken' (1,1,"b"), 
                    newStringToken' (2,2,"c"), 
                    newStringToken' (3,3,"a")
                    ]
            match source targetTags `shouldBe` GotDuplicates [newStringToken' (0,0,"a"),newStringToken' (3,3,"a")]

        it "zero intersection" $ do
            let source = [newStringToken "x", newStringToken "y"]
            match source targetTags `shouldBe` ZeroIntersection

        it "got excessive" $ do
            let source = [
                    newStringToken "a", 
                    newStringToken "b",
                    newStringToken "c",
                    newStringToken "d" -- extra
                    ]
            match source targetTags `shouldBe` GotExcessive [newStringToken "d"]

        it "missing" $ do
            let source = [ newStringToken "a", newStringToken "b"]
            match source targetTags `shouldBe` Missing ["c"]

        it "perfect match" $ do
            let source = [newStringToken "a", newStringToken "b", newStringToken "c"]
            match source targetTags `shouldBe` PerfectMatch

    describe "keli parser" $ do
        it "identifiers" $ do
            testParseKeli "_=0"
            testParseKeli "even?=0"

        it "string expr" $ do
            -- comments are just string expressions!
            testParseKeli "=\"this is a string\" pi=3.142"

        it "lambda expr" $ do
            testParseKeli "hi = x | console.log(x)"
            testParseKeli "hi = x | y | x.+(y)"

        it "multiple decl" $ do
            testParseKeli "x=5 y=5"

        it "const decl" $ do
            testParseKeli "x=5" 
            testParseKeli "=5" 

        it "monofunc decl" $ do
            testParseKeli "(this string).reverse|string=undefined"
            testParseKeli "(this string).! |string=undefined"
            testParseKeli "{a type}(x a).unit|a=undefined"
            -- testParseKeli "{a type}{b type}(x (a.with b)).invert|(b.with a)=x.second.with(x.first);"

        it "polyfunc decl" $ do
            testParseKeli "(this string).splitby(that string)|string=undefined"
            testParseKeli "(this string).replace(that string) with (the string)|string=undefined"
            testParseKeli "(this Int). == (that Int)|Int=undefined"

        it "monofunc call" $ do
            testParseKeli "=x.reverse" 
            testParseKeli "=x.!" 

        it "polyfunc call" $ do
            testParseKeli "=compiler.import(x)" 
            testParseKeli "=x.replace(a) with (b)" 
            testParseKeli "=x.+(y)" 
