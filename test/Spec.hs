import Test.Hspec
import Control.Exception (evaluate)
import Parser
import Ast
import Debug.Trace
import Analyzer
import Data.Either

testParseKeli x = 
    (case (parseKeli x) of
        Right _   -> True
        Left  err -> trace (show err) $ False) `shouldBe` True

analyze x = 
    let result = parseKeli x in
        case result of 
            Right decls -> buildDeclTable decls
            Left err    -> trace (show err) $ undefined


main :: IO ()
main = hspec $ do
    describe "keli analyzer" $ do
        it "check for duplicated id" $ do
            isLeft  (analyze "x=5\n\nx=5") `shouldBe` True
            isRight (analyze "x=5\n\ny=5") `shouldBe` True

            isRight  (analyze 
                      "x:string,up  | string=undefined \
                \ \n\n x:integer,up | string=undefined") `shouldBe` True

    describe "keli parser" $ do
        it "id starting with underscore" $ do
            testParseKeli "_=0"

        it "comments" $ do
            -- comments are just string expressions!
            testParseKeli "=\"this is a comment\"\npi=3.142"

        it "lambda expr" $ do
            testParseKeli "hi = x | console,log x"
            testParseKeli "hi = x y | x, +y"

        it "multiline decl" $ do
            testParseKeli "x=5\n\ny=5"
            testParseKeli "x=5\n\n\ny=5"
            testParseKeli "x=5\n\n\n\ny=5"

        it "const decl" $ do
            testParseKeli "x=5" 
            testParseKeli "=5" 

        it "monofunc decl" $ do
            testParseKeli "this:string,reverse|string=undefined"
            testParseKeli "this:string,! |string=undefined"
            testParseKeli "{a:type}x:a,unit|a=undefined"
            testParseKeli "{a:type b:type}x:(a,with b),invert|(b,with a)=x,second,with(x,first)"

        it "polyfunc decl" $ do
            testParseKeli "this:string,splitby that:string|string=undefined"
            testParseKeli "this:string,replace that:string with the:string|string=undefined"
            testParseKeli "this:int , == that:int|int=undefined"

        it "monofunc call" $ do
            testParseKeli "=x,reverse" 
            testParseKeli "=x,!" 

        it "polyfunc call" $ do
            testParseKeli "=compiler,import x" 
            testParseKeli "=x,replace a with b" 
            testParseKeli "=x,+ y" 