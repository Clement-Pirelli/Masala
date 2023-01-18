module Inputs.DefineInput(testInput) where
import Inputs.Framework
import Inputs.DummyNodes
import TokenType
import Node

testInput :: TestInput
testInput = 
    TestInput
    { 
        title = "define input",
        input = simpleDefineInput,
        expectedString = simpleDefineInput,
        expectedTypes = [
            TokDefine, TokName, TokName, TokPlus, TokName,
            TokDefine, TokName, TokName, TokPlus, TokName, TokStar, TokName,
            TokDefine, TokName, TokBitNot, TokName,
            TokDefine, TokName, TokBitNot, TokName, TokPlus, TokName,
            TokDefine, TokName, TokOpeningParens, TokName, TokClosingParens, TokMinus, TokName,
            TokDefine, TokName, TokOpeningParens, TokName, TokClosingParens, TokMinus, TokName,
            TokEOF
        ],
        expectedNodes = [
            dummyDefine Nothing (addition dummySymbol dummySymbol),
            dummyDefine Nothing (multiplication (addition dummySymbol dummySymbol) dummySymbol),
            dummyDefine Nothing (unOpOfType BitNot),
            dummyDefine Nothing (addition (unOpOfType BitNot) dummySymbol),
            dummyDefine (Just [dummySymbol]) (unOpOfType UMinus),
            dummyDefine Nothing (negation dummySymbol dummySymbol)
        ]
    }

simpleDefineInput :: String
simpleDefineInput = 
       "#define X A+B\n" 
    ++ "#define Y A+B*C\n"
    ++ "#define Z ~A\n"
    ++ "#define W ~A+B\n"
    ++ "#define V1(a) -a\n"
    ++ "#define V2 (a) -a"