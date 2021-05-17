#lang typed/racket
(require/typed file/sha1
               (hex-string->bytes (-> String Bytes)))
;; mil forms
;; - number
;; - hexadecimal
;; - string
;; - (let (k v k v) expr expr)
;; - (set! k v)
;; - (loop n body)
;; - #(1 2 3) (because Racket cannot distinguish square from round parens)

(: is-hex-sym? (-> Any Boolean))
(define (is-hex-sym? sym)
  (if (symbol? sym)
      (and (regexp-match #rx"^0x[0-9a-fA-F]+$" (symbol->string sym)) #t)
      #f))

(: hex-sym->bstring (-> Symbol Bytes))
(define (hex-sym->bstring hex-sym)
  (hex-string->bytes (string-replace (symbol->string hex-sym) "0x" "")))

(define-type Mil-Value
  (U Void
     Number
     Bytes
     (Listof Mil-Value)))

; hashtable matching from
(: current-mil-bindings (Parameterof (HashTable Symbol (Boxof Mil-Value))))
(define current-mil-bindings (make-parameter (ann (hash) (HashTable Symbol (Boxof Mil-Value)))))

(: mil-interp (-> Any Mil-Value))
(define (mil-interp expr)
  (match expr
    [(? integer? x) x]
    [(? is-hex-sym? x) (hex-sym->bstring (cast x Symbol))]
    [(? symbol? var-name) (unbox (hash-ref (current-mil-bindings) var-name))]
    [(? string? x) (string->bytes/utf-8 x)]
    ; let with no bindings is easy
    [`(let () ,last-expr) (mil-interp last-expr)]
    [`(let () . ,(? list? exprs)) (mil-interp (car exprs))
                                  (mil-interp `(let () . ,(cdr exprs)))]
    [`(let ,(? list? kv-pairs) . ,body)
     ; add to hashtable
     (define new-hash (foldl (lambda ((kv : Any) (accum : (HashTable Symbol (Boxof Mil-Value))))
                               (match kv
                                 [(list (? symbol? var-name)
                                        binding)
                                  (hash-set accum var-name (box (mil-interp binding)))]))
                             (current-mil-bindings)
                             (pairify kv-pairs)))
     (parameterize ([current-mil-bindings new-hash])
       (mil-interp `(let () . ,body)))]
    [`(set! ,(? symbol? var-name) ,new-value)
     (set-box! (hash-ref (current-mil-bindings) var-name)
               (mil-interp new-value))]

    [other (error "cannot recognize syntax" other)]))

(: pairify (-> (Listof Any) (Listof Any)))
(define (pairify lst)
  (match lst
    [`(,a ,b . ,c) (cons (list a b) (pairify c))]
    [`() `()]))

(mil-interp
 '(let (x 1 y 2 z 3)
    (let (z 5)
      (set! z 12345)
      z)
    z))