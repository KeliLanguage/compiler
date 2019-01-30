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

testParseKeli :: String -> Expectation
testParseKeli x = 
    (case (keliParse "<test>" x) of
        Right _   -> True
        Left  err -> trace (show err) $ False) `shouldBe` True

runTest :: IO ()
runTest = do
    testSubjects <- listDirectory "./test/specs"
    testCases <- 
        forM 
            testSubjects 
            (\subject -> do
                let dirname = "./test/specs/" ++ subject
                filenames <- listDirectory dirname
                files <- 
                    forM 
                        filenames
                        (\name -> do
                            contents <- readFile $ dirname ++ "/" ++ name
                            return (name, contents))
                return (subject, files))
    
    -- find for test cases prefixed with ONLY:
    let specificTestCases = 
            filter 
                (\(_, files) -> length files > 0)
                (map 
                    (\(subject, files) -> (subject, filter (\(filename,_) -> filename `strStartsWith` "ONLY:") files))
                    testCases)

    otherTest
    
    if length specificTestCases > 0 then
        runTest' specificTestCases
    else
        runTest' testCases

    
    
runTest' :: [(String, [(String,String)])] -> IO ()
runTest' testCases = 
    hspec $ do
        forM_ testCases
            (\(subject, files) ->
                describe subject $ do
                    forM_ files
                        (\(filename, contents) ->
                            if "====" `isInfixOf` contents then
                                let [code, expectedOutput] = split "====" contents in
                                it filename $ do
                                    if '@' `elem` filename then do
                                        result <- keliInterpret filename code 
                                        case result of 
                                            Right _ ->
                                                error "No error is thrown"
                                            Left err ->
                                                -- error (show err) -- Uncomment this line to show parse error
                                                tail (split " " (show err) !! 0) `shouldBe` strip expectedOutput
                                    else do
                                        result <- keliInterpret filename code
                                        case result of
                                            Right output ->
                                                strip output `shouldBe` strip expectedOutput
                                            Left err -> 
                                                error (show err)
                            else
                                error $ "\n\n\tERROR at " ++ filename ++ " : Each test file needs to contain ====\n\n"))

main :: IO ()
main = runTest

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
            testParseKeli "_=0;"
            testParseKeli "even?=0;"

        it "comments" $ do
            -- comments are just string expressions!
            testParseKeli "=\"this is a comment\";pi=3.142;"

        it "lambda expr" $ do
            testParseKeli "hi = x | console.log x;"
            testParseKeli "hi = x y | x.+y;"

        it "multiple decl" $ do
            testParseKeli "x=5;y=5;"

        it "const decl" $ do
            testParseKeli "x=5;" 
            testParseKeli "=5;" 

        it "monofunc decl" $ do
            testParseKeli "this:string.reverse|string=undefined;"
            testParseKeli "this:string.! |string=undefined;"
            testParseKeli "{a:type}x:a.unit|a=undefined;"
            testParseKeli "{a:type b:type}x:(a.with b).invert|(b.with a)=x.second.with(x.first);"

        it "polyfunc decl" $ do
            testParseKeli "this:string.splitby that:string|string=undefined;"
            testParseKeli "this:string.replace that:string with the:string|string=undefined;"
            testParseKeli "this:int . == that:int|int=undefined;"

        it "monofunc call" $ do
            testParseKeli "=x.reverse;" 
            testParseKeli "=x.!;" 

        it "polyfunc call" $ do
            testParseKeli "=compiler.import x;" 
            testParseKeli "=x.replace a with b;" 
            testParseKeli "=x.+ y;" 