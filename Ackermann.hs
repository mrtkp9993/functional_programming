{-
From wikipedia:
In computability theory, the Ackermann function, named after Wilhelm Ackermann,
is one of the simplest[1] and earliest-discovered examples of a total
computable function that is not primitive recursive. All primitive recursive
functions are total and computable, but the Ackermann function illustrates
that not all total computable functions are primitive recursive.
-}
module Ackermann where

import Numeric.Natural (Natural)

ackermann :: Natural -> Natural -> Natural
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))
