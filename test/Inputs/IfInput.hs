module Inputs.IfInput(testInput) where
import Inputs.Framework
import Inputs.DummyNodes
import TokenType
import Node

testInput :: TestInput
testInput =
    TestInput
    { 
        title = "if input",
        input = ifInput,
        expectedString = ifInput,
        expectedTypes = [
            TokIfdef, TokName, TokEndif,
            TokIf, TokLiteral, TokEndif,
            TokIf, TokLiteral, TokElse, TokEndif,
            TokIfdef, TokName, TokIfdef, TokName, TokEndif, TokEndif,
            TokEOF
        ],
        expectedNodes = [
            dummyIfdef [] [],
            dummyIf dummyInt [] [],
            dummyIf dummyInt [] [dummyElse []],
            dummyIfdef [dummyIfdef [] []] []
        ]
    }

ifInput :: String
ifInput = 
       "#ifdef A\n#endif\n" 
    ++ "#if 1\n#endif\n"
    ++ "#if 1\n#else\n#endif\n"
    ++ "#ifdef A\n#ifdef B\n#endif\n#endif\n"