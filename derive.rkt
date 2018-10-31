#lang racket 

;( define ( derive-x l )
;( if ( pair? l)
;( if ( number? ( car l ))
;  ;         (cons 0 ( derive-x ( cdr l)))
;          
;   (cons 1 ( derive-x ( cdr l))))
;;;;   '()))
;**************************rym*********************
(define (derive l)
  (if (list? l)
      (if (pair? l)
          (if (eq? (car l) 'x)
              '(1)
              (if (eq? (car l) '+)
                  (derive-sum (car (cdr l)) (car (cdr (cdr l))))
                  (if (eq? (car l) '*)
                      (derive-mult (car (cdr l)) (car (cdr (cdr l))))
                      
                      (derive-div (car (cdr l)) (car (cdr (cdr l))))
                      
                      )))
          '())
      (derive-simple l) ))

(define (derive-simple a)
  (if (eq? a 'x)
      1 
      0))
(define (derive-mult u v)
  (list '+  (list '*  (derive u) v)  (list '*  u  (derive v)) ))



(define (derive-sum u v)
  (list '+   (derive u) (derive v) ))
(define (derive-div u v)
  (list '/   (list '-  (list '*  (derive u)  v ) 
                   (list '*   u (derive v) )) (list '*  v v)))


(define (derive-ln l)
  (if (pair? (car  (cdr l)))
      (list '/ (derive (car (cdr l))) (cdr l))
      (list '/ (derive-simple (car (cdr l))) (cdr l))))


( define (derive-exp l)
   (if (pair? (car  (cdr l)))
       (list '* (derive (car (cdr l)))  l)
       l))


(define (derive-cos l)
  (if (pair? (car  (cdr l)))
      (list '* '-(derive (car (cdr l))) 'sin (cdr l))
      (list '- 'sin (cdr l))))


(define (derive-sin l)
  (if (pair? (car  (cdr l)))
      (list '* (derive (car (cdr l))) 'cos (cdr l))
      (list  'cos (cdr l))))
(define (derive-sqrt l)
  (if (pair? (car  (cdr l)))
      (list '/ (derive (car (cdr l))) (list '* 2 l))
      (list '/ (derive-simple (car (cdr l))) (list '* 2 l)))) 



(define (derive-general l)
  (if (list? l)
      (if (pair? l)
          (if (eq? (car l) 'ln)
              (derive-ln l)
              (if (eq? (car l) 'exp)
                  (derive-exp l)
                  (if (eq? (car l) 'cos)
                      (derive-cos l)
                      (if (eq? (car l) 'sin)
                          (derive-sin l)
                          (if (eq? (car l) 'sqrt)
                              (derive-sqrt l) 
                              (derive l))))))
          '())
      (derive-simple l)))
(define (sup-element e l)
  (if (pair? l)
      (if (list? (car l))
          (if (eq? (car (car l)) e)
              
              (cons   (sup-element e (cdr (car l))) (sup-element e (cdr l)))
              (cons (cons (car (car l)) (sup-element e (cdr (car l)))) (sup-element e (cdr l))))
          (if (eq? (car l) e)
              
              (sup-element e (cdr  l))
              (cons  (car l) (sup-element e (cdr l))))) 
      '()))

(define(derive-simplif l)
  (if (list? l)
      (if (pair? l)
          (if (pair? (cdr l))
          (if (and (eq? (car l) '+) (eq? (car (cdr l)) '0))
              (cons '+ (derive-simplif '0 (cdr (cdr l))))
              (if (and (eq? (car l) '*) (eq? (car (cdr l)) '1))
                  (cons '* (derive-simplif  (cdr (cdr  l)))) 
                  (cons (car l) (derive-simplif (cdr l)))))
          
              
                  (list (cdr l)))
          '())
      l))
          
        
