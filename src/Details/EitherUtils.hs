module Details.EitherUtils where


unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Right x) = x
unwrapEither (Left l) = error (show l)