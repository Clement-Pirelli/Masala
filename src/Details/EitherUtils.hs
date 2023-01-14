module Details.EitherUtils where


unwrapEither :: (Show a) => Either a b -> b
unwrapEither (Right x) = x
unwrapEither (Left l) = error (show l)

bothSameAnd :: (a -> a -> b) -> (c -> c -> b) -> b -> Either c a -> Either c a -> b
bothSameAnd f _ _ (Right x) (Right y) = f x y
bothSameAnd _ g _ (Left x) (Left y) = g x y
bothSameAnd _ _ e _ _ = e