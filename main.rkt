#lang racket/base
(require "private/base.rkt")
(provide log-data
         (struct-out mrlog-data)

         fresh-unique
         object->unique)

;; Components:
;; - in application process
;;   - logging library: used by apps, libs, etc to emit logs
;;   - listener: listens for log messages, stores in db
;;     - what topics?
;; - viewer: mini web server, retrieves logs, formats, displays



;; A Unique is a (probably) unique nonnegative integer.

;; FIXME:
;; A Grokker receives mrlog-data and figures out what to do with it
;; - start a new Frame (at top level or within an existing Frame)
;; - append to an existing Frame, adjust Frame data
;; - close an existing Frame (and add final summary data)


;; cf bottom-up Chart Parsing?


(define next-unique (box 0)) ;; FIXME

;; fresh-unique : -> Unique
(define (fresh-unique)
  (define next (unbox next-unique))
  (if (box-cas! next-unique next (add1 next))
      next
      (fresh-unique)))

;; process-unique : Unique
(define process-unique (fresh-unique))

;; eq=>unique : WeakHasheq[Obj => Unique]
(define eq=>unique (make-weak-hasheq))

;; object->unique : Any -> Unique
(define (object->unique obj)
  (hash-ref! eq=>unique obj (lambda () (fresh-unique))))

;; thread->unique : (U Thread ThreadDeadEvt) -> Unique
(define (thread->unique [thd (current-thread)])
  (cond [(thread? thd) (thread->unique (thread-dead-evt thd))]
        [else (object->unique thd)]))

;; log-data : ... -> Void
(define (log-data message data
                  #:process [proc-u #f]
                  #:thread [thread-u #f]
                  #:subject [subject-u #f]
                  #:timestamp [now #f])
  (define logger (current-logger))
  (when (log-level? logger 'info 'mrlog-data)
    (let ([proc-u (or proc-u process-unique)]
          [thread-u (cond [(exact-integer? thread-u) thread-u]
                          [else (thread->unique (or thread-u (current-thread)))])]
          [subject-u (or subject-u (fresh-unique))]
          [now (or now (current-inexact-milliseconds))])
      (log-message (current-logger) 'info 'mrlog-data
                   message
                   (mrlog-data proc-u thread-u subject-u now data)))))
