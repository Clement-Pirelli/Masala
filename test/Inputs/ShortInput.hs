module Inputs.ShortInput(testInput) where
import Inputs.Framework
import Inputs.DummyNodes
import TokenType

testInput :: TestInput
testInput = TestInput 
        { 
            title = "short input", 
            input = shortInput,
            expectedString = cursoredShortInput,
            expectedTypes = shortInputTypes,
            expectedNodes = [
                dummyInclude,
                dummyInclude,
                dummyInclude,
                dummyDefine (Just [dummySymbol])
                    (addition dummySymbol dummyInt),
                dummyIfdef 
                    []
                    [dummyElse []]
            ]
        }

shortInput :: String
shortInput = 
       "#include \"myOtherPath.h\"\n"
    ++ "#include <stddef.h>\n"
    ++ "#include <iostream>\n"
    ++ "\n"
    ++ "//#define A\n"
    ++ "#define B(a) \\\n"
    ++ "    a-1\n"
    ++ "\n"
    ++ "int main()\n"
    ++ "{\n"
    ++ "#ifdef A\n"
    ++ "    std::cout << B(0) << '\\n';\n"
    ++ "#else\n"
    ++ "    std::cout << \"A is not defined!\";\n"
    ++ "#endif\n"    
    ++ "}\n"

cursoredShortInput :: String
cursoredShortInput =
       "#include \"myOtherPath.h\"\n"
    ++ "#include <stddef.h>\n"
    ++ "#include <iostream>\n"
    ++ "\n"
    ++ "#define B(a)     a-1\n"
    ++ "\n"
    ++ "int main()\n"
    ++ "{\n"
    ++ "#ifdef A\n"
    ++ "    std::cout << B(0) << '\\n';\n"
    ++ "#else\n"
    ++ "    std::cout << \"A is not defined!\";\n"
    ++ "#endif\n"    
    ++ "}\n"

shortInputTypes :: [TokenType]
shortInputTypes = [
    TokInclude, TokLiteral, 
    TokInclude, TokChevronPath,
    TokInclude, TokChevronPath,
    TokDefine, TokName, TokOpeningParens, TokName, TokClosingParens,
    TokName, TokMinus, TokLiteral,
    TokIfdef, TokName,
    TokElse,
    TokEndif,
    TokEOF
    ]