{-|
Module : Utils
Authors : Carlos Infante
          Daniel Varela

Various useful common-use functions.
-}

module Utils where
import Control.Monad(zipWithM_)

-- | Produces the string "Row i, Column j".
showPos :: Int -> Int -> String
showPos 0 0 = ""
showPos i j = "Row " ++ show i ++ ", Column " ++ show j

-- | Zips three lists with a monadic action.
zipWith3M_ :: Applicative m => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
zipWith3M_ f x y z = zipWithM_ (\x' (y',z') -> f x' y' z') x (zip y z)