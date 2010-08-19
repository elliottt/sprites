module Utils where

infixl 1 >>=?
(>>=?) :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
ma >>=? k = do
  mb <- ma
  case mb of
    Nothing -> return Nothing
    Just a  -> k a
