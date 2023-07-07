# autocorrelation-collisions

This is a simple tool for calculation of periodic autocorrelation for bit
vectors and finding non-trivial sets of bit vectors which have the same
autocorrelation. A pair (X, Y) is said to be trivial if X can be obtained by
circular-shifting Y or a reverse of Y.

## Examples

**Finding non-trivial sets of length 8**

``` lisp
CL-USER> (autocorrelation-collisions:non-trivial-collisions 8)
((#*11100100 #*11011000))
```

**Calculation of autocorrelation**

``` lisp
CL-USER> (autocorrelation-collisions:autocorrelation #*11100100)
#(4 2 1 2 2 2 1 2)
CL-USER> (autocorrelation-collisions:autocorrelation #*11011000)
#(4 2 1 2 2 2 1 2)
```
