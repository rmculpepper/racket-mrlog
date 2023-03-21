#lang racket/base
(require racket/match
         db/base
         db/sqlite3
         sql
         "base.rkt")
(provide (all-defined-out))

(define db-path (build-path (find-system-path 'home-dir) "mrlog.db"))
(define (connect-db [rw? #t])
  (sqlite3-connect #:database db-path #:mode (if rw? 'create 'read-only)))

;; ----------------------------------------

(define (db-initialize! db)
  (call-with-transaction db
    (lambda ()
      (query-exec db "DROP TABLE IF EXISTS mrlog_meta")
      (query-exec db "DROP TABLE IF EXISTS mrlog_logs")
      (query-exec db
        (create-table mrlog_meta
                      #:columns [k TEXT #:not-null] [v ANY]
                      #:constraints (primary-key k)))
      (query-exec db
        (create-table mrlog_logs
                      #:columns
                      [process INTEGER]
                      [thread INTEGER]
                      [subject INTEGER]
                      [timestamp REAL]
                      [message TEXT]
                      [data TEXT]))
      (db-put-meta db "version" 0))))

(define (db-initialized? db)
  (call-with-transaction db
    (lambda ()
      (and (table-exists? db "mrlog_meta")
           (table-exists? db "mrlog_logs")
           (equal? (db-query-meta db "version") 0)))))

;; ----------------------------------------
;; Meta

(define (db-query-meta db key)
  (query-maybe-value db
    (select #:from mrlog_meta #:values v #:where (= k ,key))))

(define (db-put-meta db key value)
  (query-exec db
    (insert #:into mrlog_meta
            #:set [k ,key] [v ,value]
            #;#:or-replace)))

;; ----------------------------------------
;; Insert logs

(define (db-insert-logv db logv)
  (match logv
    [(vector level message (mrlog-data process thread subject timestamp data) 'mrlog-data)
     (begin (db-insert-log db process thread subject timestamp message data) #t)]
    [_ #f]))

(define (db-insert-log db process thread subject timestamp message data)
  (query-exec db
    (insert #:into mrlog_logs
            #:set
            [process ,process] [thread ,thread] [subject ,subject] [timestamp ,timestamp]
            [message ,message] [data ,(format "~s" data)])))

;; ----------------------------------------
;; Query logs

(define (db-get-logs db)
  (query-rows db
    (select #:from mrlog_logs
            #:values process thread subject timestamp message data
            #:order-by timestamp #:asc)))
