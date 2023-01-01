module DebugUtils where

import qualified Debug.Trace as Trace

traceStr :: Show a => [Char] -> a -> a
traceStr s x = Trace.trace (s ++ " " ++ show x) x 