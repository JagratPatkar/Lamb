#lang racket
(provide (all-defined-out))


(struct var  (string) #:transparent)  
(struct int  (num)    #:transparent) 
(struct add  (e1 e2)  #:transparent) 
(struct ifgreater (e1 e2 e3 e4)    #:transparent) 
(struct fun  (nameopt formal body) #:transparent) 
(struct call (funexp actual)       #:transparent) 
(struct mlet (var e body) #:transparent) 
(struct apair (e1 e2)     #:transparent) 
(struct fst  (e)    #:transparent) 
(struct snd  (e)    #:transparent) 
(struct aunit ()    #:transparent)
(struct isaunit (e) #:transparent) 
(struct closure (env fun) #:transparent)




(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))



(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater given non-integers")))]
        [(mlet? e) (eval-under-env (mlet-body e) (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env))]
        [(call? e) (let ([val (eval-under-env (call-funexp e) env)])
                       (if (closure? val)
                       (eval-under-env (fun-body (closure-fun val))
                                       (if (equal? #f (fun-nameopt (closure-fun val)))
                                           (cons (cons (fun-formal (closure-fun val)) (eval-under-env (call-actual e) env)) (closure-env val))
                                           (cons (cons (fun-nameopt (closure-fun val)) val) (cons (cons (fun-formal (closure-fun val)) (eval-under-env (call-actual e) env)) (closure-env val)))))
                       (error "MUPL call expected a function given a random value")))]
        [(apair? e) (let ([v1 (eval-under-env (apair-e1 e) env)]
                          [v2 (eval-under-env (apair-e2 e) env)])
                      (apair v1 v2))]
        [(fst? e) (let ([v (eval-under-env (fst-e e) env)])
                    (if (apair? v)
                        (apair-e1 v)
                    (error "MUPL fst expected a pair")))]
        [(snd? e) (let ([v (eval-under-env (snd-e e) env)])
                    (if (apair? v)
                        (apair-e2 v)
                    (error "MUPL snd expected a pair")))]
        [(isaunit? e) (let ([v (eval-under-env (isaunit-e e) env)])
                        (if (aunit? v)
                            (int 1)
                            (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))


(define (eval-exp e)
  (eval-under-env e null)) 