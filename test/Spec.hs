import Test.Hspec
import Control.Exception (evaluate)
import Parser
import Ast
import Debug.Trace

testParseKeli x = 
    (case (parseKeli x) of
        Right _   -> True
        Left  err -> trace (show err) $ False) `shouldBe` True

main :: IO ()
main = hspec $ do
    describe "keli parser" $ do
        it "lambda expr" $ do
            testParseKeli "hi = x | console,log x"

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