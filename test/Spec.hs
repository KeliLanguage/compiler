import Test.Hspec
import Control.Exception (evaluate)
import Parser
import qualified Ast.Raw as Raw
import Debug.Trace
import Analyzer
import Data.Either
import StaticError
import Keli
import System.Directory
import Control.Monad
import Data.Strings
import Data.List
import Data.String.Utils

testParseKeli x = 
    (case (parseKeli x) of
        Right _   -> True
        Left  err -> trace (show err) $ False) `shouldBe` True

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
                (\(subject, files) -> length files > 0)
                (map 
                    (\(subject, files) -> (subject, filter (\(filename,contents) -> filename `strStartsWith` "ONLY:") files))
                    testCases)
    
    if length specificTestCases > 0 then
        runTest' specificTestCases
    else
        runTest' testCases

    
    -- otherTest
    
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
                                        result <- keli' code 
                                        case result of 
                                            Right output ->
                                                error "No error is thrown"
                                            Left err ->
                                                -- error (show err) -- Uncomment this line to show parse error
                                                split " " (show err) !! 0 `shouldBe` strip expectedOutput
                                    else do
                                        result <- keli' code
                                        case result of
                                            Right output ->
                                                strip output `shouldBe` strip expectedOutput
                                            Left err -> 
                                                error (show err)
                            else
                                error $ "\n\n\tERROR at " ++ filename ++ " : Each test file needs to contain ====\n\n"))

main = runTest

otherTest :: IO ()
otherTest = hspec $ do
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