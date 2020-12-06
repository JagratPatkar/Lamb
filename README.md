<p align="center">
  <img   src="https://github.com/JagratPatkar/Lamb/blob/main/images/Lamb.png"/>
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
   * *Closures*
   * *First Class Functions*
   * *Higher Order Functions*
   * *Currying*
* __Arithmetic Operations__
  * *Add*


## Getting Started

#### Clone the repo in a local directory
```
git https://github.com/JagratPatkar/Lamb.git 
```
#### Create a file with .lmb extension
```
touch my1.lmb
```

#### Add the following code to the file

```
(+ 10 20)
```

#### Using Lamb
```
./Lamb my1.lmb
```

#### Output

```
$30
```

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
