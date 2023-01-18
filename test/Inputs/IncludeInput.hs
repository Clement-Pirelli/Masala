module Inputs.IncludeInput(testInput) where
import Inputs.Framework
import Inputs.DummyNodes
import TokenType

testInput = TestInput
    {
        title = "include input",
        input = includeInput,
        expectedString = includeInput,
        expectedTypes = includeInputTypes,
        expectedNodes = [
            dummyInclude,
            dummyEmptyDefine Nothing,
            dummyInclude,
            dummyEmptyDefine Nothing
        ]
    }

includeInput :: String
includeInput =
       "#include <hello_world.h>\n"
    ++ "#define A\n"
    ++ "#include \"howdy_fellas.h\"\n"
    ++ "#define B"

includeInputTypes :: [TokenType]
includeInputTypes = [
    TokInclude, TokChevronPath,
    TokDefine, TokName,
    TokInclude, TokLiteral, 
    TokDefine, TokName, 
    TokEOF
    ]