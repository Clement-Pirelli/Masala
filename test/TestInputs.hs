module TestInputs where

import TokenType

data TestInput = TestInput { title :: String, input :: String, expectedString :: String, expectedTypes :: [TokenType] }


inputs :: [TestInput]
inputs = 
    [
        TestInput { title = "short input", input = shortInput, expectedString = cursoredShortInput, expectedTypes = shortInputTypes},
        TestInput { title = "long input", input = longInput, expectedString = cursoredLongInput, expectedTypes = longInputTypes},
        TestInput { title = "include input", input = includeInput, expectedString = cursoredIncludeInput, expectedTypes = includeInputTypes},
        TestInput { title = "single comment input", input = singleCommentInput, expectedString = cursoredSingleCommentInput, expectedTypes = singleCommentInputTypes},
        TestInput { title = "single comment start input", input = singleCommentStartInput, expectedString = cursoredSingleCommentStartInput, expectedTypes = singleCommentStartInputTypes}
    ]


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

includeInput :: String
includeInput =
       "#include <hello_world.h>\n"
    ++ "#define A\n"
    ++ "#include \"howdy_fellas.h\"\n"
    ++ "#define B"

cursoredIncludeInput :: String
cursoredIncludeInput = includeInput

includeInputTypes :: [TokenType]
includeInputTypes = [
    TokInclude, TokChevronPath,
    TokDefine, TokName,
    TokInclude, TokLiteral, 
    TokDefine, TokName, 
    TokEOF
    ]

singleCommentInput :: String
singleCommentInput =
       "#define A\n"
    ++ "//Howdy partner!\n"
    ++ "#define B"

cursoredSingleCommentInput :: String
cursoredSingleCommentInput = 
       "#define A\n"
    ++ "#define B"

singleCommentInputTypes :: [TokenType]
singleCommentInputTypes = [
    TokDefine, TokName,
    TokDefine, TokName,
    TokEOF
    ]

singleCommentStartInput :: String
singleCommentStartInput =
       "//Howdy partner!\n"
    ++ "#define A\n"
    ++ "#define B"

cursoredSingleCommentStartInput :: String
cursoredSingleCommentStartInput = 
       "#define A\n"
    ++ "#define B"

singleCommentStartInputTypes :: [TokenType]
singleCommentStartInputTypes = [
    TokDefine, TokName,
    TokDefine, TokName,
    TokEOF
    ]