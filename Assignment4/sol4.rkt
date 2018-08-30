;;Kusdavletov Ernar, 20152008

#lang racket
(provide (all-defined-out)) ;; exports the defined variables in this file.

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;;Part A
(define (racketlist->mupllist racket_list)
  (cond [(null? racket_list) (aunit)] 
        [(null? (cdr racket_list)) (apair (car racket_list) (aunit))]
        [#t (apair (car racket_list) (racketlist->mupllist (cdr racket_list)))]))

;;Part B
(define (mupllist->racketlist mupl_list)  
  (cond [(equal? (aunit) mupl_list) null]
        [(equal? (apair-e2 mupl_list) (aunit)) (cons (apair-e1 mupl_list) null)]
        [#t (append (list (apair-e1 mupl_list)) (mupllist->racketlist (apair-e2 mupl_list)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) (envlookup env (var-string e))]
        ;; addition
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; constant 
        [(int? e) e]
        ;; closure
        [(closure? e) e]
        ;; aunit 
        [(aunit? e) e]
        ;; apair
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        ;; function
        [(fun? e) (closure env e)]
        ;; ifgreater
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (or (not (int? v1)) (not (int? v2)))
               (error "MUPL ifgreater applied to non-int")
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))))]
        ;; mlet
        [(mlet? e)
         (let* ([v1 (eval-under-env (mlet-e e) env)]
                [new-env (append env (list (cons (mlet-var e) v1)))])
           (eval-under-env (mlet-body e) new-env))]
        ;; call
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
	       [v2 (eval-under-env (call-actual e) env)])
	   (if (closure? v1) 
               (let* ([function (closure-fun v1)]
                      [fun-name (fun-nameopt function)]
                      [fun-arg  (fun-formal  function)]
                      [fun-code (fun-body    function)]
                      [old-env  (closure-env v1)]
                      [new-env  (append old-env (list (cons fun-arg v2))
                                        (if fun-name
                                            (list (cons fun-name v1))
                                            null))])
	       (eval-under-env fun-code new-env))
	       (error "MUPL call applied to non-closure")))]
        ;; fst
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]
        ;; snd
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-apair")))]
        ;; isaunit
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (= (length lstlst) 0) e2
      (let* ([hd (car lstlst)]
             [tl (cdr lstlst)]
             [name (car hd)]
             [exp  (cdr hd)])
        (mlet name exp (mlet* tl e2)))))

(define (ifeq e1 e2 e3 e4) (ifgreater e1 e2 (ifgreater e2 e1 e3 e4) (ifgreater e2 e1 e4 e3)))

;; Problem 4

(define mupl-map
  (fun "mupl-map" "f"
       (fun #f "xs"
            (ifaunit (var "xs")
                     (aunit)
                     (mlet* (list (cons "head" (fst (var "xs")))
                                  (cons "tail" (snd (var "xs")))
                                  (cons "first"  (call (var "f") (var "head")))
                                  (cons "second" (call (call (var "mupl-map") (var "f")) (var "tail"))))
                            (apair (var "first") (var "second")))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "mupl_map"
             (fun "f" "xs"
                  (call (call (var "map") (fun #f "x" (add (var "x") (var "mupl_map")))) (var "xs"))))))



(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))