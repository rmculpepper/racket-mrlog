#lang racket/base
(require "main.rkt")

(define (count n)
  (cond [(zero? n)
         (log-data "hit zero" #f)]
        [else
         (log-data "count" (list n))
         #;(sleep 0.4)
         (printf "~a...\n" n)
         (count (sub1 n))]))

(count 10)

;; Wait for logs to be propagated (FIXME?)
(sleep 0.1)
