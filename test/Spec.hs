import Test.Hspec
import Control.Exception (evaluate)
import Parser
import Ast
import Debug.Trace
import Analyzer
import Data.Either
import SymbolTable
import StaticError

testParseKeli x = 
    (case (parseKeli x) of
        Right _   -> True
        Left  err -> trace (show err) $ False) `shouldBe` True

analyze x = 
    let decls  = parseKeli x in
    case decls of 
        Right decls -> 
            case buildSymTab decls of
                Right symtab -> Right (analzyeAst decls symtab)
                Left err -> Left err 
        Left err -> trace (show err) $ undefined
        

main :: IO ()
main = hspec $ do
    describe "keli analyzer" $ do
        it "check for duplicated const id" $ do
            (case analyze "x=5;x=5;" of Left KErrorDuplicatedId -> True;_->False) `shouldBe` True
            isRight (analyze "x=5;y=5;") `shouldBe` True


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