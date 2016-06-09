module ThomasAlgorithm exposing (..)

import Util exposing (..)
import Array exposing (Array)
import Tuple2 

-- Solve the  n x n  tridiagonal system for y:
--
--  [ a(1)  c(1)                                  ] [  y(1)  ]   [  f(1)  ]
--  [ b(2)  a(2)  c(2)                            ] [  y(2)  ]   [  f(2)  ]
--  [       b(3)  a(3)  c(3)                      ] [        ]   [        ]
--  [            ...   ...   ...                  ] [  ...   ] = [  ...   ]
--  [                    ...    ...    ...        ] [        ]   [        ]
--  [                        b(n-1) a(n-1) c(n-1) ] [ y(n-1) ]   [ f(n-1) ]
--  [                                 b(n)  a(n)  ] [  y(n)  ]   [  f(n)  ]
--
--  f must be a vector (row or column) of length n
--  a, b, c must be vectors of length n (note that b(1) and c(n) are not used)

unzip : Array (a,b) -> (Array a, Array b)
unzip = Array.toList >> List.unzip >> Tuple2.mapEach Array.fromList Array.fromList

 
tridiag: Array Float -> Array Float -> Array Float -> Array Float -> Array(Array Float)
tridiag a b c d  =
    --init 
    let n = Array.length a
        f = Array.initialize n (always 0.0)
        c' = setAt 0 ((getAt 0 c)/(getAt 0 b)) c
    --forward sweep
        (c'',d') = unzip <| Array.map (forwardSweep a b c' d) <| Array.fromList [1..n]
    --reverse sweep
        f' = setAt (n-1) (getAt (n-1) d') f 
    in Array.map (reverseSweep c'' d' f') <| Array.fromList [0..(n-1)]

forwardSweep: Array Float -> Array Float -> Array Float -> Array Float -> Int -> (Float,  Float)
forwardSweep  a b c d i = 
    let m = 1.0 / ((getAt i b) - (getAt i a) * (getAt (i-1) c))
        c' = (getAt i c) * m
        d' = ((getAt i d) - (getAt i a) * (getAt (i-1) d)) * m
    in (c',d')

reverseSweep: Array Float -> Array Float -> Array Float -> Int -> Array Float
reverseSweep c d' f i = 
    setAt i ((getAt i d') - ((getAt i c) * (getAt (i+1) f))) f