module Main where

import Tokeniser (scanTokens)
import Parser (parseTokens)
import Text.Pretty.Simple(pPrint)


main :: IO ()
main = do
    let nodes = (parseTokens . scanTokens) simpleDefineInput
    pPrint nodes


simpleDefineInput :: String
simpleDefineInput = 
       "#define X A+B\n" 
    ++ "#define Y A+B*C\n"
    ++ "#define Z ~A\n"
    ++ "#define W ~A+B\n"
    ++ "#define V1(a) -a\n"
    ++ "#define V2 (a) -a"

tokeniserInput :: String
tokeniserInput = 
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

tokeniserShortInput :: String
tokeniserShortInput = 
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