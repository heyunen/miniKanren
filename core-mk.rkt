#lang scheme

;; Core miniKanren
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax var
  (syntax-rules ()
    ((_ x) (vector x))))

(define-syntax var?
  (syntax-rules ()
    ((_ x) (vector? x))))

(define empty-s '())

(define ext-s-no-check
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define lookup
  (lambda (v s)
    (cond
      [(var? v)
       (let [(a (assq v s))]
         (cond
           [a (cdr a)]
           [else v]))]
      [else v])))

(define walk
  (lambda (v s)
    (cond
      [(var? v)
       (let [(a (assq v s))]
         (cond
           [a (walk (cdr a) s)]
           [else v]))]
      [else v])))

(define ext-s
  (lambda (x v s)
    (cond
      ((occurs x v s) #f)
      (else (ext-s-no-check x v s)))))

(define occurs
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (eq? v x))
        ((pair? v) (or (occurs x (car v) s) (occurs x (cdr v) s)))
        (else #f)))))

(define unify
  (lambda (u v s)
    (let ((u (walk u s))
          (v (walk v s)))
      (cond
        ((eq? u v) s)
        ((var? u)
         (cond
           ((var? v) (ext-s-no-check u v s))
           (else (ext-s u v s))))
        ((var? v) (ext-s v u s))
        ((and (pair? u) (pair? v))
         (let ((s (unify (car u) (car v) s)))
           (and s (unify (cdr u) (cdr v) s))))
        ((equal? u v) s)
        (else #f)))))

(define reify
  (lambda (v s)
    (let ((v (walk∗ v s)))
      (walk∗ v (reify-s v empty-s)))))

(define walk∗
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) v)
        ((pair? v) (cons (walk∗ (car v) s) (walk∗ (cdr v) s)))
        (else v)))))

(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (ext-s v (reify-name (length s)) s))
        ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
        (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol (string-append "_." (number->string n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(let ([x (var 'x)] [y (var 'y)])
  (walk x (ext-s x y (ext-s y 5 empty-s))))