#lang racket/base
(require racket/match
         racket/class
         db/base
         db/sqlite3
         "private/base.rkt"
         "private/db.rkt")

;; Default: sqlite3 database at $HOME/mrlog.db

;; ------------------------------------------------------------

(define (handle-logs db receiver)
  (define logv1 (sync receiver))
  (define logvs
    (let loop ([rlogvs (list logv1)] [n 1])
      (cond [(and (< n 10) (sync/timeout 0 receiver))
             => (lambda (logv) (loop (cons logv rlogvs) (add1 n)))]
            [else (reverse rlogvs)])))
  (handle-some-logs db logvs)
  (handle-logs db receiver))

(define (handle-some-logs db logvs)
  (eprintf "handling ~s logs\n" (length logvs))
  (call-with-transaction db
    (lambda ()
      (for ([logv (in-list logvs)])
        (db-insert-logv db logv)))))

;; ------------------------------------------------------------

(define db
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (log-mrlog-error (exn-message e))
                     #f)])
    (define db (connect-db))
    (plumber-add-flush! (current-plumber) (lambda (ph) (send db disconnect)))
    (unless (db-initialized? db) (db-initialize! db))
    db))

(when db
  (define receiver (make-log-receiver (current-logger) 'debug 'mrlog-data))
  (void (thread (lambda () (handle-logs db receiver)))))
