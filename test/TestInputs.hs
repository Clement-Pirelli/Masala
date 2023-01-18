module TestInputs where

import Inputs.Framework
import qualified Inputs.CommentInputs as Comment
import qualified Inputs.IncludeInput as Include
import qualified Inputs.ShortInput as Short
import qualified Inputs.LongInput as Long
import qualified Inputs.DefineInput as Define
import qualified Inputs.IfInput as If

inputs :: [TestInput]
inputs = Comment.testInputs ++
    [
        Include.testInput,
        Short.testInput,
        Long.testInput,
        Define.testInput,
        If.testInput
    ]