newtype Pkt a = Pkt (a, a)

instance (Show a) => Show (Pkt a) where
  show :: Show a => Pkt a -> String
  show (Pkt (x, y)) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Functor Pkt where
  fmap :: (a -> b) -> Pkt a -> Pkt b
  fmap f (Pkt (x, y)) = Pkt (f x, f y)

instance Applicative Pkt where
  pure :: a -> Pkt a
  pure x = Pkt (x, x)

  (<*>) :: Pkt (a -> b) -> Pkt a -> Pkt b
  Pkt (f, g) <*> Pkt (x, y) = Pkt (f x, g y)

instance Monad Pkt where
  return :: a -> Pkt a
  return = pure

  (>>=) :: Pkt a -> (a -> Pkt b) -> Pkt b
  Pkt (x, y) >>= f =
    let Pkt (m, _) = f x
        Pkt (_, n) = f y
     in Pkt (m, n)
