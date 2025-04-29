module BenchMarks where

fib = \n -> case n of
                0 -> 0
                1 -> 1
                _ -> fib (n - 2) + fib (n - 1)


-- fibInline :: Int -> Int
-- fibInline = fib
-- fibNoInline :: Int -> Int
-- fibNoInline = fib

{-# INLINE fibInline #-}
fibInline :: Int -> Int
fibInline 0 = 0
fibInline 1 = 1
fibInline n = fibInline (n - 2) + fibInline (n - 1)

{-# NOINLINE fibNoInline #-}
fibNoInline :: Int -> Int
fibNoInline 0 = 0
fibNoInline 1 = 1
fibNoInline n = fibNoInline (n - 2) + fibNoInline (n - 1)
