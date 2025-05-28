module QP where

import Quipper

main :: IO ()
main = do
    print_simple ASCII $ plus_minus False

plus_minus :: Bool -> Circ Qubit
plus_minus b = do
    q <- qinit b
    r <- hadamard q
    pure r

