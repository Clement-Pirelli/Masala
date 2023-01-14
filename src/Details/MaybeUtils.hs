module Details.MaybeUtils where

bothSameAnd :: (a -> a -> b) -> b -> b -> Maybe a -> Maybe a -> b
bothSameAnd f _ _ (Just x) (Just y) = f x y
bothSameAnd _ d _ Nothing Nothing = d
bothSameAnd _ _ e _ _ = e