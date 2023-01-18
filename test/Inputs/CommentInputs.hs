module Inputs.CommentInputs(testInputs) where
import Inputs.Framework
import Inputs.DummyNodes
import TokenType

testInputs :: [TestInput]
testInputs = [
    TestInput
    { 
        title = "single comment input",
        input = singleCommentInput,
        expectedString = cursoredSingleCommentInput,
        expectedTypes = singleCommentInputTypes,
        expectedNodes = [
            dummyEmptyDefine Nothing,
            dummyEmptyDefine Nothing
        ]
    },
    TestInput 
    {
        title = "single comment start input",
        input = singleCommentStartInput,
        expectedString = cursoredSingleCommentStartInput,
        expectedTypes = singleCommentStartInputTypes,
        expectedNodes = [
            dummyEmptyDefine Nothing,
            dummyEmptyDefine Nothing
        ]
    }]


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