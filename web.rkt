#lang racket/base
(require racket/match
         racket/runtime-path
         racket/tcp
         web-server/servlet
         web-server/servlet-env
         json
         "private/db.rkt")
(provide (all-defined-out))

;; FIXME
(define db (connect-db))

(define-values (dispatch _make-url)
  (dispatch-rules
   [("logs")
    #:method "get"
    (lambda (req)
      (define logvs (db-get-logs db))
      (ok-response
       #"text/plain"
       (lambda (out)
         (for ([logv (in-list logvs)])
           (match logv
             [(vector process thread subject timestamp message data-s)
              (fprintf out "~s ~a ~s\n" timestamp message (read (open-input-string data-s)))])))))]
   ))

(define (ok-response content-type data)
  (response/output
   #:code 200
   #:mime-type content-type
   (lambda (out)
     (cond [(string? data) (write-string data out)]
           [(procedure? data) (data out)]))))

;; ============================================================

(define PORT 17888)

(define-runtime-path static-dir "web-content")

(define (start [log? #f] [browser? #f])
  (serve/servlet dispatch
                 #:port PORT
                 #:servlet-regexp #rx""
                 #:command-line? #t
                 #:launch-browser? browser?
                 #:extra-files-paths (list (path->string static-dir))
                 #:log-file (if log? "/dev/stdout" #f)))

;; ============================================================

(module+ main
  (start #t #f))
