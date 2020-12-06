<p align="center">
  <img   src="https://github.com/JagratPatkar/Lamb/blob/main/images/Lamb.png"/>
</p>


##
_Dynamically Typed and interpreted __Functional Programming Language___, implemented in __Ocaml__, and is a subset of the Lisp family of programming languages.

## Features
Some of the basic features 
* __Data Types__
  * *Int*
  * *Pair*
  * *List*
* __Functional Idioms__
   * *Lambdas*
   * *Functions* 
   * *Closures*
   * *First Class Functions*
   * *Higher Order Functions*

## Examples

### Church Encodings

```racket
(define TRUE x (lambda y x))

(define FALSE x (lambda y y))

(define NOT b (call (call b FALSE) TRUE))

(define CONJUNCTION x 
                    (lambda y (call (call x y) FALSE)))

(define DISJUNCTION x 
                    (lambda y (call (call x TRUE) y)))

(define BEQ x 
            (lambda y (call 
                        (call x (call (call y TRUE) FALSE)) 
                        (call x (call (call y FALSE) TRUE)))))
```
