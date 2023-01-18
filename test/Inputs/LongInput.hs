module Inputs.LongInput(testInput) where
import Inputs.Framework
import Inputs.DummyNodes
import TokenType

testInput :: TestInput
testInput = TestInput 
        {
            title = "long input",
            input = longInput,
            expectedString = cursoredLongInput,
            expectedTypes = longInputTypes,
            expectedNodes = [
                dummyInclude,
                dummyInclude,
                dummyDefine Nothing
                    (lessThan 
                        (division 
                            (addition 
                                dummyInt 
                                (negation dummyInt dummyInt)) 
                            dummyInt) 
                        dummyInt),
                dummyIfdef 
                    [] 
                    [dummyElse [
                        dummyIf
                            (dummyFunc [dummySymbol])
                            [] 
                            [],
                        dummyIf
                            (dummyFunc [dummySymbol])
                            []
                            [dummyElse []]
                    ]]
            ]
        }


longInput :: String
longInput =
    "#include <iostream>\n"
    ++ "#include \"myPath.h\"\n"
    ++ "\n"
    ++ "#define A ((1+2-3) / 4) < 10\n"
    ++ "#ifdef MY_DEFINE\n"
    ++ "\n"
    ++ "//stuff\n"
    ++ "//stuff\n"
    ++ "int main()\n"
    ++ "{\n"
    ++ "    stuff;\n"
    ++ "    return 0;\n"
    ++ "}\n"
    ++ "\n"
    ++ "#else\n"
    ++ "\n"
    ++ "int main(\n"
    ++ "#if defined(MY_OTHER_DEFINE)\n"
    ++ "    int argc, char **argv\n"
    ++ "#endif\n"
    ++ ")\n"
    ++ "{\n"
    ++ "#if defined(MY_OTHER_DEFINE)\n"
    ++ "    for(int i = 0; i < argc; i++)\n"
    ++ "    {\n"
    ++ "        std::cout << argv[i];\n"
    ++ "    }\n"
    ++ "    return 0;\n"
    ++ "#else\n"
    ++ "    return 1;\n"
    ++ "#endif\n"
    ++ "}\n"
    ++ "//stuff\n"
    ++ "\n"
    ++ "#endif\n"

cursoredLongInput :: String
cursoredLongInput =
    "#include <iostream>\n"
    ++ "#include \"myPath.h\"\n"
    ++ "\n"
    ++ "#define A ((1+2-3) / 4) < 10\n"
    ++ "#ifdef MY_DEFINE\n"
    ++ "\n"
    ++ "int main()\n"
    ++ "{\n"
    ++ "    stuff;\n"
    ++ "    return 0;\n"
    ++ "}\n"
    ++ "\n"
    ++ "#else\n"
    ++ "\n"
    ++ "int main(\n"
    ++ "#if defined(MY_OTHER_DEFINE)\n"
    ++ "    int argc, char **argv\n"
    ++ "#endif\n"
    ++ ")\n"
    ++ "{\n"
    ++ "#if defined(MY_OTHER_DEFINE)\n"
    ++ "    for(int i = 0; i < argc; i++)\n"
    ++ "    {\n"
    ++ "        std::cout << argv[i];\n"
    ++ "    }\n"
    ++ "    return 0;\n"
    ++ "#else\n"
    ++ "    return 1;\n"
    ++ "#endif\n"
    ++ "}\n"
    ++ "\n"
    ++ "#endif\n"


longInputTypes :: [TokenType]
longInputTypes = [
    TokInclude, TokChevronPath,
    TokInclude, TokLiteral, 
    TokDefine, TokName, TokOpeningParens, TokOpeningParens, TokLiteral, TokPlus, TokLiteral, TokMinus, TokLiteral, TokClosingParens, TokSlash, TokLiteral, TokClosingParens, TokOpeningChevron, TokLiteral,
    TokIfdef, TokName,
    TokElse,
    TokIf, TokName, TokOpeningParens, TokName, TokClosingParens,
    TokEndif,
    TokIf, TokName, TokOpeningParens, TokName, TokClosingParens,
    TokElse,
    TokEndif,
    TokEndif,
    TokEOF
    ]
