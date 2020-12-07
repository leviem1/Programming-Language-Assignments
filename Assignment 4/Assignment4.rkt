#lang racket

(require 2htdp/planetcute)

;;; Get a list of numbers starting at high and decreasing by step until they are less then or equal to low
;;; Accepts:
;;; step - the amount to decrement by
;;; high - the number to start at
;;; low - the number to start at
;;; Returns a list of numbers starting at high and decreasing by step until they are less then or equal to low
(define (downseries step high low)
  (if (>= high low)
      (cons high (downseries step (- high step) low))
      '()))

;;; Add "meow" to the end of each string in lst
;;; Accepts:
;;; lst - the list of strings
;;; Returns a list of strings with "meow" appended onto them
(define (meow-string-map lst)
  (map (lambda (str) (string-append str "meow")) lst))

;;; Gets the item at the index of the result of the quotient of n divided by the length of lst
;;; Accepts:
;;; lst - the list
;;; n - the number to divide by the length of the list
;;; Returns the value at the index of the result of the quotient of n divided by the length of lst
(define (list-ref-div lst n)
  (cond
    [(< n 0) (error "list-ref-div: negative number")]
    [(empty? lst) (error "list-ref-div: empty list")]
    [else (list-ref lst (floor (/ n (length lst))))]))

;;; Cheatsy way of next-k-items implementation
;;; (define (next-k-items s k)
;;;   (stream->list (stream-take s k)))

;;; Get the next k items of s by "iterating" over the stream k times
;;; Accepts:
;;; s - the stream
;;; k - the number of items to get
;;; Returns a list of the next k items
(define (next-k-items s k)
  (cond
    [(> k 0) (cons (car (s)) (next-k-items (cdr (s)) (- k 1)))]
    [else '()]))

;;; Get the kth item of s by "iterating" over the stream k times
;;; Accepts:
;;; s - the stream
;;; k - the number of the item to get
;;; Returns the kth item
(define (kth-item s k)
  (cond
    [(> k 0) (kth-item (cdr (s)) (- k 1))]
    [else (car (s))]))

;;; Negate numbers divisible by two or five using remainders in a conditional
(define negate-2-and-5
  (letrec ([func (lambda (x)
                   (if (or (= (remainder x 2) 0) (= (remainder x 5) 0))
                       (cons (* -1 x) (lambda () (func (+ x 1))))
                       (cons x (lambda () (func (+ x 1))))))])
    (lambda () (func 1))))

;;; Alternate between heart, key, and star using a modulo in a cond to pick which one to show
(define key-heart-star
  (letrec ([func (lambda (x)
                   (cond
                     [(= (modulo x 3) 0) (cons heart (lambda () (func (+ x 1))))]
                     [(= (modulo x 3) 1) (cons key (lambda () (func (+ x 1))))]
                     [(= (modulo x 3) 2) (cons yellow-star (lambda () (func (+ x 1))))]))])
    (lambda () (func 0))))

;;; Get a stream wgere all items are a pair of (2 . k) where k is the next item in s
;;; Accepts:
;;; s - the stream
;;; Returns a stream where all items are a pair of (2 . k) where k is the next item in s
(define (two-pairs-stream s)
  (letrec ([func (lambda (x)
                   (lambda () (cons (cons 2 (car (x))) (func (cdr (x))))))])
    (func s)))

(define (spin-stream xs ys)
  (letrec ([func (lambda (x)
                   (lambda () (cons (cons (list-ref xs (modulo x (length xs))) (list-ref ys (modulo x (length ys)))) (func (+ x 1)))))])
    (func 0)))
