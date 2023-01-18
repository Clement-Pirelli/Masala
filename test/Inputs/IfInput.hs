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
            TokIf, TokName, TokOpeningParens, TokName, TokClosingParens, TokEndif,
            TokIf, TokName, TokOpeningParens, TokName, TokClosingParens, TokAnd, TokName, TokOpeningParens, TokName, TokClosingParens, TokEndif,
            TokIfdef, TokName, TokIfdef, TokName, TokEndif, TokEndif,
            TokIfdef, TokName,
            TokIfdef, TokName,
            TokElse,
            TokEndif,
            TokElse,
            TokIfdef, TokName,
            TokElse,
            TokEndif,
            TokEndif,
            TokEOF
        ],
        expectedNodes = [
            dummyIfdef [] [],
            dummyIf dummyInt [] [],
            dummyIf dummyInt [] [dummyElse []],
            dummyIf dummyDefined [] [],
            dummyIf (binOpOfType And dummyDefined dummyDefined) [] [],
            dummyIfdef [dummyIfdef [] []] [],
            dummyIfdef [dummyIfdef [] []] [dummyElse [dummyIfdef [] []]]
        ]
    }

ifInput :: String
ifInput = 
       "#ifdef A\n#endif\n" 
    ++ "#if 1\n#endif\n"
    ++ "#if 1\n#else\n#endif\n"
    ++ "#if defined(X)\n#endif\n"
    ++ "#if defined(X) && defined(Y)\n#endif\n"
    ++ "#ifdef A\n#ifdef B\n#endif\n#endif\n"
    ++ nestedIfs

nestedIfs :: String
nestedIfs = 
       "#ifdef MY_DEFINE\n"
    ++  "#ifdef MY_OTHER_DEFINE\n"
    ++  "#else\n"
    ++  "#endif\n"
    ++ "#else\n"
    ++  "#ifdef AAAA\n"
    ++  "#else\n"
    ++  "#endif\n"
    ++ "#endif"