import Test.Hspec
import Control.Exception (evaluate)
import Parser
import Ast
import Debug.Trace
import Analyzer
import Data.Either
import SymbolTable
import StaticError
import Keli

testParseKeli x = 
    (case (parseKeli x) of
        Right _   -> True
        Left  err -> trace (show err) $ False) `shouldBe` True

testAnalyze x = 
    let decls  = parseKeli x in
    case decls of 
        Right decls -> 
            case buildSymTab decls of
                Right symtab -> Right (analyze symtab)
                Left err -> Left err 
        Left err -> trace (show err) $ undefined

getBaseCode = readFile "./kelilib/base.keli"

main :: IO ()
main = hspec $ do
    describe "keli exec" $ do
        it "case 1" $ do
            keli' "animal=record.name \"dog\" age 5;"

    describe "keli analyzer" $ do
        it "check for duplicated const id" $ do
            (case testAnalyze "x=5;x=5;" of Left (KErrorDuplicatedId _) -> True;_->False) `shouldBe` True
            isRight (testAnalyze "x=5;y=5;") `shouldBe` True

        it "keli record" $ do
            baseCode <- getBaseCode
            isRight (testAnalyze (baseCode ++ "animal=record.name str age int;")) `shouldBe` True
            isRight (testAnalyze (baseCode ++ "dog=record.name \"dog\" age 9;")) `shouldBe` True
        
        it "keli func" $ do
            baseCode <- getBaseCode
            -- TODO: Complete the test
            putStrLn (show (testAnalyze (baseCode ++ "x:int.+y:int|int=undefined;")))

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