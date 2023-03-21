#lang racket/base
(provide (all-defined-out))

(define-logger mrlog)

;; A MrLogData is:
(struct mrlog-data
  (process-u    ;; Unique
   thread-u     ;; Unique
   subject-u    ;; Unique/#f
   timestamp    ;; Real (current-inexact-milliseconds)
   data         ;; Datum
   ) #:prefab)
