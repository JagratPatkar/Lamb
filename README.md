<p align="center">
  <img  height="200" width="600" src="https://github.com/JagratPatkar/Lamb/blob/main/images/Lamb%20(1).svg"/>
</p>


##
_Dynamically Typed and interpreted __Functional Programming Language___, implemented in __Ocaml__. __Lamb__ is a subset of the Lisp family of programming languages.

## Features
Some of the basic features 
* __Data Types__
  * *Int*
  * *Pair*
  * *List*
* __Functional Idioms__
   * *Lambdas*
   * *Functions* 
   * *Recursion*
   * *Closures*
   * *First Class Functions*
   * *Higher Order Functions*
   * *Currying*
   * *Let Expressions*
* __Arithmetic Operations__
  * *Add*
  * *Sub*


## Examples

### Church Encodings

```racket

;True 
(define TRUE x (lambda y x))


;False
(define FALSE x (lambda y y))


;Negation
(define NOT b (call (call b FALSE) TRUE))


;And or Conjunction
(define CONJUNCTION x 
                    (lambda y (call (call x y) FALSE)))


;Or / Disjunction
(define DISJUNCTION x 
                    (lambda y (call (call x TRUE) y)))
                    
                    
;Boolean Equality
(define BEQ x 
            (lambda y (call 
                        (call x (call (call y TRUE) FALSE)) 
                        (call x (call (call y FALSE) TRUE)))))
```


### Map 

```racket
(define map f (define helper xs 
                                (cmp (unit? xs) 0
                                     xs
                                     (pair (call f (car xs)) 
                                           (call helper (cdr xs)))))) 
 ```
