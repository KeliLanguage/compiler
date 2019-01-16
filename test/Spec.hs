import Test.Hspec
import Control.Exception (evaluate)
import Parser
import Ast
import Debug.Trace
import Analyzer
import Data.Either
import StaticError
import Keli
import System.Directory
import Control.Monad

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

    hspec $ do
        forM_ testCases
            (\(subject, files) ->
                describe subject $ do
                    forM_ files
                        (\(filename, contents) ->
                            it filename $ do
                                if head filename == '@' then
                                    keli' contents `shouldThrow` anyException
                                else
                                    keli' contents))
    
    otherTest

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