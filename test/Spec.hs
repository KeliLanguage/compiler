import Test.Hspec
import Control.Exception (evaluate)
import Parser
import Ast

testParseKeli x = 
    (case (parseKeli x) of
        Right _ -> True
        Left  _ -> False) `shouldBe` True

main :: IO ()
main = hspec $ do
    describe "keli parser" $ do
        it "const decl" $ do
            testParseKeli "x=5" 
        
        it "id-less const decl" $ do
            testParseKeli "=5"

        it "monofunc decl (word)" $ do
            testParseKeli "this:string,reverse->string=undefined"

        it "monofunc decl (operator)" $ do
            testParseKeli "this:string,!!->string=undefined"

        it "polyfunc decl (word) 1" $ do
            testParseKeli "this:string,splitby that:string->string=undefined"

        it "polyfunc decl (word) 2" $ do
            testParseKeli "this:string,replace that:string with the:string->string=undefined"

        it "polyfunc decl (operator) 1" $ do
            testParseKeli "this:int, + that:int->int=undefined"

        it "monofunc call (word)" $ do
            testParseKeli "=x,reverse" 

        it "monofunc call (operator)" $ do
            testParseKeli "=x,!" 

        it "polyfunc call 1 (word)" $ do
            testParseKeli "=compiler,import x" 

        it "polyfunc call 2 (word)" $ do
            testParseKeli "=x,replace a with b" 

        it "polyfunc call (operator)" $ do
            testParseKeli "=x,+ y" 