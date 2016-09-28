;; bitstirng module implements the subset of Erlang bit syntax.

(module bitstring
  (bitmatch
   bitpacket
   bitconstruct
   bitstring-pattern-continue
   make-bitstring
   bitstring?
   bitstring-length
   ->bitstring
   vector->bitstring
   u8vector->bitstring
   string->bitstring
   blob->bitstring
   bitstring-read
   bitstring-share
   bitstring-seek
   bitstring-create
   bitstring-reserve
   bitstring=?
   bitstring-append
   bitstring-append!
   bitstring-not
   bitstring-bit-set?
   bitstring-reverse
   bitstring->list
   bitstring->blob
   bitstring->string
   bitstring->vector
   bitstring->u8vector
   bitstring->integer
   bitstring->integer-big
   bitstring->integer-little
   bitstring->integer-host
   integer->bitstring
   integer->bitstring-big
   integer->bitstring-little
   integer->bitstring-host
   bitstring-start
   bitstring-end
   bitstring-buffer
   bitstring-getter
   bitstring->half
   bitstring->single
   single->bitstring
   bitstring->double
   double->bitstring
   list->bitstring)

  (import scheme chicken extras)

(use bitstring-lowlevel
     srfi-1
     srfi-4)

(define-syntax symbol??
  (er-macro-transformer
    (lambda (e r c)
      (let* ((args (cdr e))
      	     (name (car args))
      	     (yes (cadr args))
      	     (no (caddr args)))
      	(if (symbol? name) yes no)))))

; (expand-value x char str int)
(define-syntax expand-value
  (er-macro-transformer
    (lambda (e r c)
      (let* ((args (cdr e))
             (name (car args))
             (char-branch (cadr args))
             (string-branch (caddr args))
             (integer-branch (cadddr args)))
        (cond
          ((char? name) char-branch)
          ((string? name) string-branch)
          ((integer? name) integer-branch)
          (else (error "invalid value" `name)))))))

(define-syntax bitpacket-def-fields
  (syntax-rules ()
    ((_ name fields ...)
      (define-syntax name
        (er-macro-transformer
         ;;(name mode stream handler PREFIX rest ...)
         (lambda (e r c)
           (define (rename-with-prefix prefix pat)
             (let* ((n (length pat))
                    (rename (lambda (sym)
                              (string->symbol (string-append (symbol->string prefix)
                                                             "." (symbol->string sym))))))
               (or (>= n 1)
                   (syntax-error "invalid bitpacket field pattern" pat))
               (cond ((and (>= n 1) (symbol? (first pat)))
                      (cons (rename (first pat)) (cdr pat)))
                     (else pat))))
           (let* ((args    (cdr    e))
                  (mode    (first  args))
                  (stream  (second args))
                  (handler (third  args))
                  (prefix  (fourth args))
                  (rest    (drop   args 4)))
             (or (symbol? prefix)
                 (equal? prefix #f)
                 (syntax-error "invalid bitpacket prefix" prefix))
             ;; inline bitpacket fields
             `(bitstring-pattern-continue ,mode
                                          ,stream
                                          ,handler
                                          ,(if prefix
                                               (map (lambda (pat) (rename-with-prefix prefix pat)) '(fields ...))
                                               '(fields ...))
                                          ,rest))))))))

(define-syntax bitpacket
  (syntax-rules ()
    ((_ (name constructor) fields ...)
     (begin
       (bitpacket-def-fields name fields ...)
       (define-syntax constructor
         (syntax-rules ()
           ((_ . args)
            (let args
              (bitconstruct (name bitpacket))))))))
    ((_ name fields ...)
     (bitpacket-def-fields name fields ...))))


(define-syntax bitstring-pattern-continue
  (syntax-rules ()
    ((_ mode stream handler (fields ...) (rest ...))
      (bitstring-pattern mode stream handler fields ... rest ...))))

(define-syntax make-bitmatch-result
  (syntax-rules () ((_ (handler ...))
                    (list (begin handler ...)))))

(define-syntax bitmatch-result
  (syntax-rules () ((_ result)
                    (car result))))

(define-syntax bitconstruct
  (syntax-rules ()
    ((_ patterns ...)
      (let ((bstr (bitstring-reserve 64)))
        (bitstring-pattern "write" bstr "no-handler" patterns ...)))))

(define-syntax bitmatch
  (syntax-rules ()
    ((_ value . patterns)
      (let ((bstr (->bitstring value)))
        (bitmatch-result (bitmatch-pattern-list bstr patterns))))))

(define-syntax bitmatch-pattern-list
  (syntax-rules (else ->)
    ((_ bstr ())
      (error 'bitstring-match-failure))
    ((_ bstr ((else . handler)))
      (make-bitmatch-result handler))
    ; short syntax form (pat ... patX -> exp)
    ((_ bstr ((pattern ... -> handler) . rest))
      (or (bitmatch-pattern bstr (handler) (pattern ...))
          (bitmatch-pattern-list bstr rest)))
    ((_ bstr ((pattern . handler) . rest))
      (or (bitmatch-pattern bstr handler pattern)
          (bitmatch-pattern-list bstr rest)))))

(define-syntax bitmatch-pattern
  (syntax-rules ()
    ((_ bstr handler (pattern ...))
      ; shared copy of bitstring instance
      (let ((stream (bitstring-share bstr (bitstring-start bstr) (bitstring-end bstr))))
        (bitstring-pattern "read" stream handler pattern ...)))))

(define-syntax bitstring-malformed-pattern
  (syntax-rules ()
    ((bitstring-malformed-pattern)
     (syntax-error "bitstring-malformed-pattern"))))

(define-syntax bitstring-pattern
  (syntax-rules (big little host bitstring check float double bitpacket signed unsigned boolean offset seek)
    ; all patterns take expansion
    ((_ "read" stream handler)
      (and
        ; ensure that no more bits left
        (zero? (bitstring-length stream))
        (make-bitmatch-result handler)))
    ((_ "write" stream handler)
      stream)
    ; zero-length bitstring
    ((_ "read" stream handler ())
      (and
        (zero? (bitstring-length stream))
        (make-bitmatch-result handler)))
    ((_ "write" stream handler ())
      stream)
    ; user guard expression
    ((_ mode stream handler (check guard) rest ...)
      (and
        guard
        (bitstring-pattern mode stream handler rest ...)))
    ; evaluate constructing function
    ((_ "write" stream handler ((VALUE ...) bitstring) rest ...)
      (and-let* ((tmp (VALUE ...))
                 (bits (bitstring-length tmp)))
        (bitstring-pattern "write" stream handler (tmp bits bitstring) rest ...)))
    ; evaluate reader procedure (PROC) -> returns #f or (list num-bits-readed any-value)
    ((_ "read" stream handler ((name PROC ...) bitstring) rest ...)
     (and-let* ((tmp (->bitstring stream))
                (res (PROC ... tmp))
                (bits (first res)) ; length
                (name (second res))) ; value
       (bitstring-pattern "read" stream handler (tmp bits bitstring) rest ...)))
    ; tell current stream offset
    ((_ "read" stream handler (NAME offset) rest ...)
     (let ((NAME (bitstring-start stream)))
       (bitstring-pattern "read" stream handler rest ...)))
    ; move current stream offset
    ((_ "read" stream handler (OFFS seek) rest ...)
     (and-let* ((with-offset (bitstring-seek stream OFFS)))
       (bitstring-pattern "read" with-offset handler rest ...)))
    ; bitpacket
    ((_ mode stream handler (NAME bitpacket) rest ...)
      (NAME mode stream handler #f rest ...))
    ; bitpacket with prefix
    ((_ mode stream handler (PREFIX NAME bitpacket) rest ...)
     (NAME mode stream handler PREFIX rest ...))
    ; allow in bitconstruct dont type length
    ((_ "write" stream handler (NAME bitstring) rest ...)
      (bitstring-pattern-expand "write" stream NAME
        (bitstring-pattern "write" stream handler rest ...)))
    ; greedy bitstring
    ((_ mode stream handler (NAME bitstring))
      (bitstring-pattern-expand mode stream NAME
        (bitstring-pattern mode stream handler)))
    ; boolean
    ((_ mode stream handler (NAME boolean) rest ...)
     (bitstring-pattern-expand mode stream NAME 8 (boolean big unsigned)
       (bitstring-pattern mode stream handler rest ...)))
    ; boolean bits
    ((_ mode stream handler (NAME BITS boolean) rest ...)
     (bitstring-pattern-expand mode stream NAME BITS (boolean big unsigned)
       (bitstring-pattern mode stream handler rest ...)))
    ; boolean bits endian
    ((_ mode stream handler (NAME BITS boolean ENDIAN) rest ...)
     (bitstring-pattern-expand mode stream NAME BITS (boolean ENDIAN unsigned)
       (bitstring-pattern mode stream handler rest ...)))
    ; double 64
    ((_ mode stream handler (NAME double) rest ...)
      (bitstring-pattern-expand mode stream NAME 64 (float big)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME double ENDIAN) rest ...)
      (bitstring-pattern-expand mode stream NAME 64 (float ENDIAN)
        (bitstring-pattern mode stream handler rest ...)))
    ; float 32
    ((_ mode stream handler (NAME float) rest ...)
      (bitstring-pattern-expand mode stream NAME 32 (float big)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME float ENDIAN) rest ...)
      (bitstring-pattern-expand mode stream NAME 32 (float ENDIAN)
        (bitstring-pattern mode stream handler rest ...)))
    ; generic float bits
    ((_ mode stream handler (NAME BITS float) rest ...)
     (bitstring-pattern-expand mode stream NAME BITS (float big)
       (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME BITS float ENDIAN) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (float ENDIAN)
        (bitstring-pattern mode stream handler rest ...)))
    ; bigendian
    ((_ mode stream handler (NAME BITS big) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (big unsigned)
        (bitstring-pattern mode stream handler rest ...)))
    ; littleendian
    ((_ mode stream handler (NAME BITS little) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (little unsigned)
        (bitstring-pattern mode stream handler rest ...)))
    ; same endianness as host
    ((_ mode stream handler (NAME BITS host) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (host unsigned)
        (bitstring-pattern mode stream handler rest ...)))
    ; bitstring
    ((_ mode stream handler (NAME BITS bitstring) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS bitstring
        (bitstring-pattern mode stream handler rest ...)))
    ; integer attibutes
    ((_ mode stream handler (NAME BITS signed) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (big signed)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME BITS unsigned) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (big unsigned)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME BITS signed ENDIAN) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (ENDIAN signed)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME BITS unsigned ENDIAN) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (ENDIAN unsigned)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME BITS ENDIAN SIGNED) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS (ENDIAN SIGNED)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME signed) rest ...)
      (bitstring-pattern-expand mode stream NAME 8 (big signed)
        (bitstring-pattern mode stream handler rest ...)))
    ((_ mode stream handler (NAME unsigned) rest ...)
      (bitstring-pattern-expand mode stream NAME 8 (big unsigned)
        (bitstring-pattern mode stream handler rest ...)))
    ; rewrite by default to (NAME BITS (big unsigned))
    ((_ mode stream handler (NAME BITS) rest ...)
      (bitstring-pattern mode stream handler (NAME BITS big) rest ...))
    ; rewrite immidiate value
    ((_ mode stream handler (NAME) rest ...)
      (symbol?? NAME
        ; yes
        (bitstring-pattern mode stream handler (NAME 8 big) rest ...)
        ; no
        (bitstring-pattern-value mode stream handler (NAME) rest ...)))
    ; dismiss other pattern forms
    ((_ mode stream handler . rest)
     (bitstring-malformed-pattern rest))))

(define-syntax bitstring-pattern-value
  (syntax-rules ()
    ((_ mode stream handler (VALUE) rest ...)
      (expand-value VALUE
        ; char
        (bitstring-pattern mode stream handler ((char->integer VALUE) 8 big) rest ...)
        ; string
        (bitstring-pattern mode stream handler
          (VALUE (* 8 (string-length VALUE)) bitstring) rest ...)
        ; integer
        (bitstring-pattern mode stream handler (VALUE 8 big) rest ...)))))

(define-syntax bitstring-pattern-expand
  (syntax-rules ()
    ((_ "write" stream name continuation)
      (and-let* ((tmp (->bitstring name)))
        ;(print "write-expand:" `stream " name:" `name)
      	(bitstring-append! stream tmp)
      	continuation))
    ((_ "write" stream name bits type continuation)
      (and-let* ((tmp (bitstring-write-expand name bits type)))
        ;(print "write-expand:" `stream " name:" `name)
      	(bitstring-append! stream tmp)
      	continuation))
    ((_ "read" stream name continuation) ; read all rest bytes
      (symbol?? name
      	(and-let* ((name (bitstring-read stream (bitstring-length stream))))
          ;(print "read-expand: " `(name bits type) " rest: " `continuation)
      	  continuation)
        (syntax-error "not a symbol name" `name)))
    ((_ "read" stream name bits type continuation)
      (and-let* ((tmp (bitstring-read stream bits)))
             (symbol?? name
               (let ((name (bitstring-read-expand tmp bits type)))
                  continuation)
               (and (optimize-compare tmp name bits type)
                    continuation))))))

(define-syntax optimize-compare
  (syntax-rules ()
    ((_ tmp value bits (ENDIAN SIGNED))
     (= value (bitstring-read-integer tmp bits ENDIAN SIGNED)))
    ((_ tmp value bits type)
     (bitstring=? tmp (bitstring-write-expand value bits type)))))

(define-syntax float-reorder-bytes
  (syntax-rules (little big host)
    ((_ host tmp)
     (cond-expand
       (little-endian (float-reorder-bytes little tmp))
       (else (float-reorder-bytes big tmp))))
    ((_ little tmp)
     (cond-expand
       (little-endian tmp)
       (else (bitstring-reverse tmp 8))))
    ((_ big tmp)
     (cond-expand
       (little-endian (bitstring-reverse tmp 8))
       (else tmp)))))

(define-syntax bitstring-read-expand
  (syntax-rules (bitstring float boolean)
    ((_ tmp 32 (float ENDIAN))
     (bitstring->single (float-reorder-bytes ENDIAN tmp)))
    ((_ tmp 64 (float ENDIAN))
     (bitstring->double (float-reorder-bytes ENDIAN tmp)))
    ((_ tmp bits (boolean ENDIAN SIGNED))
     (not (zero? (bitstring-read-integer tmp bits ENDIAN SIGNED))))
    ((_ tmp bits (ENDIAN SIGNED))
     (bitstring-read-integer tmp bits ENDIAN SIGNED))
    ((_ tmp bits bitstring)
     tmp))) ; return bitstring as is

(define-syntax bitstring-read-integer
  (syntax-rules (big little host signed unsigned)
    ((_ tmp bits big signed)
     (if (bitstring-bit-set? tmp 0)
       (- (+ 1 (bitstring->integer-big (bitstring-not tmp))))
       (bitstring->integer-big tmp)))
    ((_ tmp bits little signed)
     (if (bitstring-bit-set? tmp (if (< bits 8) (sub1 bits) (- bits 8)))
       (- (+ 1 (bitstring->integer-little (bitstring-not tmp))))
       (bitstring->integer-little tmp)))
    ((_ tmp bits host signed)
      (cond-expand
        (little-endian (bitstring-read-integer tmp bits little signed))
        (else (bitstring-read-integer tmp bits big signed))))
    ((_ tmp bits big unsigned)
      (bitstring->integer-big tmp))
    ((_ tmp bits little unsigned)
      (bitstring->integer-little tmp))
    ((_ tmp bits host unsigned)
      (bitstring->integer-host tmp))
    ((_ tmp bits ENDIAN SIGNED)
      (syntax-error "invalid integer attibute" `ENDIAN `SIGNED))))

(define-syntax bitstring-write-expand
  (syntax-rules (bitstring float boolean)
    ((_ tmp 32 (float ENDIAN))
     (float-reorder-bytes ENDIAN (single->bitstring tmp)))
    ((_ tmp 64 (float ENDIAN))
     (float-reorder-bytes ENDIAN (double->bitstring tmp)))
    ((_ tmp bits (boolean ENDIAN SIGNED))
     (bitstring-write-integer (if tmp 1 0) bits ENDIAN SIGNED))
    ((_ tmp bits (ENDIAN SIGNED))
      (bitstring-write-integer tmp bits ENDIAN SIGNED))
    ((_ tmp bits bitstring)
      (if (bitstring? tmp)
      	tmp
        (->bitstring tmp)))))

(define-syntax bitstring-write-integer
  (syntax-rules (big little host signed unsigned)
    ((_ tmp bits big signed)
      (integer->bitstring-big tmp bits))
    ((_ tmp bits little signed)
      (integer->bitstring-little tmp bits))
    ((_ tmp bits host signed)
      (integer->bitstring-host tmp bits))
    ((_ tmp bits big unsigned)
      (integer->bitstring-big tmp bits))
    ((_ tmp bits little unsigned)
      (integer->bitstring-little tmp bits))
    ((_ tmp bits host unsigned)
      (integer->bitstring-host tmp bits))
    ((_ tmp bits ENDIAN SIGNED)
      (syntax-error "invalid integer attibute" `ENDIAN `SIGNED))))


;;;;;;;;;;;;;;;;;;;;;;
;; bitstring

(define-record bitstring
  start   ; buffer offset in bits
  end     ; buffer offset in bits
  buffer  ; any container with random access
  getter  ; (lambda (buffer index) -> byte)
  setter) ; (lambda (buffer index byte) -> void)

(define-record-printer (bitstring x out)
  (fprintf out "<bitstring ~A ~A ~A>"
    (bitstring-start x) (bitstring-end x) (bitstring-buffer x)))

(define (bitstring-length bs)
  (- (bitstring-end bs) (bitstring-start bs)))

; compute space required for {{n}} bits
(define (space-required n)
  (+ (quotient n 8) (if (zero? (remainder n 8)) 0 1)))

(define (bitstring-create)
  (bitstring-reserve 128))

(define (bitstring-reserve size-in-bits)
  (let ((size (space-required size-in-bits)))
    (make-bitstring 0 0 (make-u8vector size 0) u8vector-ref u8vector-set!)))

(define (string->bitstring s)
  (make-bitstring 0 (* 8 (string-length s)) s
    (lambda (str index) (char->integer (string-ref str index)))
    (lambda (str index byte) (string-set! str index (integer->char byte)))))

(define (vector->bitstring v)
  (make-bitstring 0 (* 8 (vector-length v)) v vector-ref vector-set!))

(define (u8vector->bitstring v)
  (make-bitstring 0 (* 8 (u8vector-length v)) v u8vector-ref u8vector-set!))

(define (blob->bitstring b)
  (u8vector->bitstring (blob->u8vector/shared b)))

(define (->bitstring x)
  (cond
    ((bitstring? x)
      (bitstring-share x (bitstring-start x) (bitstring-end x)))
    ((u8vector? x)
      (u8vector->bitstring x))
    ((string? x)
      (string->bitstring x))
    ((vector? x)
      (vector->bitstring x))
    ((blob? x)
      (u8vector->bitstring (blob->u8vector/shared x)))
    (else
      (error "bitstring-invalid-value" x))))

(define (bitstring->blob bs #!optional (zero-extending 'left))
  (u8vector->blob/shared (bitstring->u8vector bs zero-extending)))

(define (bitstring->u8vector bs #!optional (zero-extending 'left))
             ; make bs copy for mutable bitstring-read
  (let loop ((bs (bitstring-share bs (bitstring-start bs) (bitstring-end bs)))
             (n (bitstring-length bs))
             (index 0)
             (tmp (make-u8vector (space-required (bitstring-length bs)))))
    (cond
      ((zero? n)
        tmp)
      ((< n 8)
       (let ((byte (bitstring->integer-big (bitstring-read bs n))))
          (u8vector-set! tmp index (if (eq? zero-extending 'left)
                                        byte
                                       (fxshl byte (- 8 n))))
          tmp))
      (else
         (u8vector-set! tmp index (bitstring->integer-big (bitstring-read bs 8)))
         (loop bs (- n 8) (add1 index) tmp)))))

(define (bitstring->string bs)
  (list->string (map integer->char (bitstring->list bs 8))))

(define (bitstring->vector bs)
  (list->vector (bitstring->list bs 8)))

(define (bitstring->list bs #!optional (bits 1) (endian 'big))
  (bitstring->listn bs bits endian))

(define (bitstring->listn bs bits endian)
  (let loop ((bs (->bitstring bs)); make copy for mutable bitstring-read
             (n (bitstring-length bs))
             (acc (list)))
    (cond ((zero? n)
           (reverse acc))
          ((< n bits)
           (loop bs 0
               (cons (bitstring->integer (bitstring-read bs n) endian) acc)))
          (else
           (loop bs (- n bits)
               (cons (bitstring->integer (bitstring-read bs bits) endian) acc))))))

(define (list->bitstring lst #!optional (bits 1) (endian 'big))
  (let loop ((rest lst)
             (acc (bitstring-reserve (* (length lst) bits))))
    (if (null-list? rest)
      acc
      (loop (cdr rest) (bitstring-append! acc (integer->bitstring (car rest) bits endian))))))

(define (bitstring-reverse bs #!optional (bits 1) (endian 'big))
  (list->bitstring (reverse (bitstring->list bs bits endian)) bits endian))

(define (bitstring=? a b)
  (and
    (= (bitstring-length a) (bitstring-length b))
    (if (and (bytestring? a) (bytestring? b))
      (bytestring=? a b)
      (equal? (bitstring->list a 8) (bitstring->list b 8)))))

(define (bytestring? bs)
  (and (zero? (remainder (bitstring-start bs) 8))
       (zero? (remainder (bitstring-length bs) 8))))

(define (bytestring=? a b)
  (let ((alen (quotient (bitstring-length a) 8))
        (blen (quotient (bitstring-length b) 8))
        (e (quotient (bitstring-end a) 8)))
    (and (= alen blen)
      (let loop ((i (quotient (bitstring-start a) 8))
                 (j (quotient (bitstring-start b) 8)))
        (if (< i e)
          (if (= (bitstring-load-byte a i)
                 (bitstring-load-byte b j))
            (loop (add1 i) (add1 j))
               #f)
          #t)))))

(define (bitstring-load-byte bs index)
  ((bitstring-getter bs) (bitstring-buffer bs) index))

(define (bitstring-store-byte bs index value)
  ((bitstring-setter bs) (bitstring-buffer bs) index value))

; extract {{count}} bits starting from {{offset}}, {{value}} should'be 8 bit integer.
(define-inline (extract-bits value offset count)
  (fxshr (fxand (fxshl value offset) #xFF)
         (- 8 count)))

(define (bitstring-fold proc init bs)
  (let loop ((start (bitstring-start bs))
             (end (bitstring-end bs))
             (index (quotient (bitstring-start bs) 8))
             (drift (remainder (bitstring-start bs) 8))
             (count (- 8 (remainder (bitstring-start bs) 8)))
             (acc init))
    (let ((n (min (- end start) count)))
      (if (<= n 0)
        acc
        (loop (+ start n) end
              (add1 index) ; move index
              0 ; reset drift
              8 ; setup 8 bit chunk
              (proc (extract-bits (bitstring-load-byte bs index) drift n) n acc))))))

(define (bitstring-not bs)
  (let ((len (bitstring-length bs))
        (tmp (bitstring->u8vector bs 'right)))
    (u8vector-not tmp (u8vector-length tmp))
    (make-bitstring 0 len tmp u8vector-ref u8vector-set!)))

(define (bitstring-bit-set? bs n)
  (let ((start (bitstring-start bs))
        (end (bitstring-end bs)))
    (let* ((index (if (negative? n)
                    (+ end n)
                    (+ start n)))
           (byte-index (quotient index 8))
           (bit-index (- 7 (remainder index 8))))
      (if (and (<= start index) (< index end))
        (bit-set? (bitstring-load-byte bs byte-index) bit-index)
        (error "out of range" start end n)))))

(define (bitstring->integer-big bs)
  (bitstring-fold
    (lambda (value count result)
      (bitwise-ior (arithmetic-shift result count) value))
    0
    bs))

(define (bitstring->integer-little bs)
  (car (bitstring-fold
         (lambda (value count acc)
           (let ((result (car acc))
                 (shift (cdr acc)))
             (cons (bitwise-ior result (arithmetic-shift value shift))
                   (+ shift count))))
         (cons 0 0)
         bs)))

(define (integer->bitstring-little value count)
  (let loop ((start 0)
             (n (min count 8))
             (bs (bitstring-reserve count)))
    (bitstring-end-set! bs count)
    (if (<= count start)
      bs
      (let ((x (bitwise-and (arithmetic-shift value (- start)) 255)))
        (bitstring-store-byte bs (quotient start 8) (fxshl x (- 8 n)))
        (loop (+ start n) (min (- count start n) 8) bs)))))

(define (integer->bitstring-big value count)
  (let loop ((start count)
             (n (min count 8))
             (bs (bitstring-reserve count)))
    (bitstring-end-set! bs count)
    (if (<= start 0)
      bs
      (let ((x (bitwise-and (arithmetic-shift value (- n start)) 255)))
        (bitstring-store-byte bs (quotient (- count start) 8) (fxshl x (- 8 n)))
        (loop (- start n) (min start 8) bs)))))

(define (bitstring->integer bitstring endian)
  (case endian
    ((big)
      (bitstring->integer-big bitstring))
    ((little)
      (bitstring->integer-little bitstring))
    ((host)
      (bitstring->integer-host bitstring))
    (else
      (error "invalid endian value" `endian))))

(define bitstring->integer-host
  (cond-expand
    (little-endian bitstring->integer-little)
    (else bitstring->integer-big)))

(define integer->bitstring-host
  (cond-expand
    (little-endian integer->bitstring-little)
    (else integer->bitstring-big)))

(define (integer->bitstring value count endian)
  (case endian
    ('little
      (integer->bitstring-little value count))
    ('host
      (integer->bitstring-host value count))
    (else
      (integer->bitstring-big value count))))

(define (bitstring->half bs)
  (let ((s (bitstring-read bs 1))
        (e (bitstring-read bs 5))
        (m (bitstring-read bs 10)))
    (make-half-float
      (bitstring->integer-big s)
      (bitstring->integer-big e)
      (bitstring->integer-big m))))

(define (make-half-float signbit exponent mantissa)
  ;(newline)
  ;(print "s: " signbit " e: " exponent " m: " mantissa)
  (cond
    ((and (zero? exponent) (zero? mantissa))
      (if (zero? signbit) +0. -0.))
    ((= exponent 31)
      (if (zero? mantissa)
      	(if (zero? signbit) +inf.0 -inf.0)
      	(if (zero? signbit) +nan.0 -nan.0)))
    (else
      (let ((e (- exponent 15))
      	    (m (bitwise-ior #x400 mantissa)))
      	(let loop ((i 10) (s 1.) (f 0.))
      	  (let* ((x (arithmetic-shift 1 i))
      	         (b (bitwise-and m x)))
      	    (if (or (zero? i))
      	      (* f (expt 2 e) (if (zero? signbit) 1. -1.))
      	      (loop (- i 1) (/ s 2) (if (zero? b) f (+ f s))))))))))

(define (single->bitstring value)
    (let ((buf (make-u8vector 4)))
        (float->uint32 buf value)
        (->bitstring buf)))

(define (double->bitstring value)
    (let ((buf (make-u8vector 8)))
        (double->uint64 buf value)
        (->bitstring buf)))

(define (bitstring->single bs)
    (uint32->float (bitstring->blob bs)))

(define (bitstring->double bs)
    (uint64->double (bitstring->blob bs)))

(define (bitstring-share bs from to)
  (make-bitstring from to (bitstring-buffer bs) (bitstring-getter bs) (bitstring-setter bs)))

(define (bitstring-seek bs offs)
  (let ((from (+ (bitstring-start bs) offs))
        (to (bitstring-end bs)))
    (and (<= 0 from)
         (<= from to)
         (bitstring-share bs from to))))

(define (bitstring-read bs n)
  (let ((from (bitstring-start bs))
        (to (+ (bitstring-start bs) n)))
    (and (<= to (bitstring-end bs))
      (let ((bs/shared (bitstring-share bs from to)))
        (bitstring-start-set! bs to)
        bs/shared))))

(define (bitstring-buffer-size bs)
  (let ((buffer (bitstring-buffer bs)))
    (* 8 ; return size in bits
      (cond
      	((u8vector? buffer)
      	  (u8vector-length buffer))
      	((string? buffer)
      	  (string-length buffer))
      	(else
      	  (error "not implemented for this buffer type"))))))

(define (bitstring-buffer-resize bs size-in-bits)
  (let* ((new-size (space-required size-in-bits))
         (tmp (make-u8vector new-size 0))
         (used (bitstring-buffer-size bs)))
    (let copy ((i 0)
    	       (e (quotient used 8)))
      (when (< i e)
        (u8vector-set! tmp i (bitstring-load-byte bs i))
        (copy (+ i 1) e)))
    ; replace buffer with accessors
    (bitstring-buffer-set! bs tmp)
    (bitstring-getter-set! bs u8vector-ref)
    (bitstring-setter-set! bs u8vector-set!)))

(define (bitstring-required-length args)
  (fold
    (lambda (bs len)
      (+ len (bitstring-length bs)))
    0
    args))

(define (bitstring-append . args)
  (fold
    (lambda (bs acc)
      (bitstring-append! acc bs))
    (bitstring-reserve (bitstring-required-length args))
    args))

(define (bitstring-append! dst . args)
  (fold
    (lambda (bs acc)
      (bitstring-append2! acc bs))
    dst
    args))

(define (bitstring-append2! dest src)
  ; need ensure that dest buffer long enough
  (let ((required (bitstring-length src))
        (position (bitstring-end dest))
        (reserved (bitstring-buffer-size dest)))
    (when (< (- reserved position) required)
      (bitstring-buffer-resize dest
        (+ reserved (inexact->exact (* 0.50 reserved)) required)))
    (bitstring-fold
      (lambda (value nbits acc)
        (bitstring-append-safe! acc (fxshl value (- 8 nbits)) nbits))
      dest
      src)))

(define (bitstring-append-safe! bs value nbits)
  (let* ((position (bitstring-end bs))
         (index (quotient position 8))
         (drift (remainder position 8)))
    (if (zero? drift)
      ; store aligned
      (begin
        (bitstring-store-byte bs index value)
        (bitstring-end-set! bs (+ position nbits)))
      ; store unaligned
      (let ((byte-src (bitstring-load-byte bs index))
            (byte-dst (fxshr value drift))
      	    (restbits (- 8 drift)))
        (bitstring-store-byte bs index (fxior byte-src byte-dst))
      	; store rest bits if didnt fit in current byte
      	(if (< restbits nbits)
          (bitstring-store-byte bs (+ index 1) (fxshl value restbits)))
        (bitstring-end-set! bs (+ position nbits))))
    bs));return bitstring

);module
