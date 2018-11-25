module Utils.EitherUtils (inverseEither) where

inverseEither :: Either a b -> Either b a
inverseEither (Right x) = Left x
inverseEither (Left x) = Right x