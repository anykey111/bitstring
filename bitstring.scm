;; bitstirng module implements the subset of Erlang bit syntax.

(module bitstring
  (bitmatch
   bitpacket
   bitconstruct
   bitstring-pattern-continue
   make-bitstring
   bitstring?
   bitstring-length
   bitstring-of-any
   bitstring-of-vector
   bitstring-read
   bitstring-share
   bitstring-compare
   bitstring-append 
   bitstring-create
   bitstring->list
   bitstring->integer-big
   bitstring->integer-little
   integer->bitstring-big
   integer->bitstring-little
   bitstring-offset
   bitstring-numbits
   bitstring-buffer
   bitstring->half
   bitstring->single)

  (import scheme chicken extras srfi-4)
  (use srfi-4)

(define-syntax symbol??
  (er-macro-transformer
    (lambda (e r c)
      (let* ((args (cdr e))
      	     (name (car args))
      	     (yes (cadr args))
      	     (no (caddr args)))
      	(if (symbol? name) yes no)))))

(define-syntax value-type??
  (er-macro-transformer
    (lambda (e r c)
      (let* ((args (cdr e))
      	     (name (car args)))
      	(cond
      	  ((integer? name)
      	    #\I)
      	  ((char? name)
      	    #\C)
      	  ((string? name)
      	    #\S)
      	  ((symbol? name)
      	    #\X)
      	  (else
      	    #\?))))))

(define-syntax bitpacket
  (syntax-rules ()
    ((_ name fields ...)
      (define-syntax name
      	(er-macro-transformer
      	  ;; (name context args ...)
      	  (lambda (e r c)
      	    (let ((context (cadr e))
      	    	  (args (cddr e)))
      	      ;; inline packet fields
      	      `(bitstring-pattern-continue ,context (fields ...) ,args)))
      	  )))))

(define-syntax bitmatch
  (syntax-rules ()
    ((_ value patterns ...)
      (call-with-current-continuation
	(lambda (return)
	  (or (bitstring-constructor ("secret" "matching" value return) patterns ...)))))))

(define-syntax bitconstruct
  (syntax-rules ()
    ((_ patterns ...)
      (call-with-current-continuation
	(lambda (return)
	  (or (bitstring-constructor ("secret" "constructing" return) patterns ...)))))))

(define-syntax bitstring-constructor
  (syntax-rules (else ->)
    ;; constructing syntax
    ((_ ("secret" "constructing" return))
      (abort (list 'bitstring-match-failure)))
    ((_ ("secret" "constructing" return) (else expression))
      (return expression))
    ((_ ("secret" "constructing" return) (pattern ...) rest ...)
      (or
      	(let ((stream (bitstring-create)))
      	  ;(print "group: " `(pattern ...))
      	  (bitstring-pattern ("secret" "constructing" stream (return stream)) pattern ...))
      	(bitstring-constructor ("secret" "constructing" return) rest ...)))
    ;; matching syntax
    ((_ ("secret" "matching" value return))
      (abort (list 'bitstring-match-failure)))
    ((_ ("secret" "matching" value return) (else expression))
      (return expression))
    ((_ ("secret" "matching" value return) (pattern ... -> expression) rest ...)
      ; short form
      (bitstring-constructor
      	("secret" "matching" value return) ((pattern ...) expression) rest ...))
    ((_ ("secret" "matching" value return) ((pattern ...) expression) rest ...)
      (or
      	(let ((stream (bitstring-of-any value)))
      	  ;(print "group: " `(pattern ...))
      	  (bitstring-pattern ("secret" "matching" stream (return expression)) pattern ...))
      	(bitstring-constructor ("secret" "matching" value return) rest ...)))))

(define-syntax bitstring-packet-expand
  (syntax-rules ()
    ((_ mode stream handler name)
      (name ("secret" mode stream handler)))
    ((_ mode stream handler name rest ...)
      (name ("secret" mode stream handler) rest ...))))

(define-syntax bitstring-pattern-continue
  (syntax-rules ()
    ((_ context (fields ...) (rest ...))
      (bitstring-pattern context fields ... rest ...))))

(define-syntax bitstring-pattern
  (syntax-rules (big little bitstring check float bitpacket)
    ;user handler
    ((_ ("secret" "constructing" stream handler))
      handler)
    ((_ ("secret" "matching" stream handler))
      ; ensure that no more bits left
      (if (zero? (bitstring-length stream))
      	handler
      	#f))
    ; zero-length bitstring
    ((_ ("secret" mode stream handler) ())
      (zero? (bitstring-length stream)))
    ; user guard expression
    ((_ ("secret" mode stream handler) (check condition) rest ...)
      (and
      	condition
      	(bitstring-pattern ("secret" mode stream handler) rest ...)))
    ; evaluate constructing function
    ((_ ("secret" "constructing" stream handler) ((VALUE ...) bitstring) rest ...)
      (and-let* ((tmp (VALUE ...))
      	         (bits (bitstring-length tmp)))
	(bitstring-pattern ("secret" "constructing" stream handler)
	                   (tmp bits bitstring) rest ...)))
    ; bitpacket
    ((_ ("secret" mode stream handler) (NAME bitpacket) rest ...)
      (bitstring-packet-expand mode stream handler NAME rest ...))
    ; bitpacket at tail
    ((_ ("secret" mode stream handler) (NAME bitpacket))
      (bitstring-packet-expand mode stream handler NAME))
    ; greedy bitstring
    ((_ ("secret" mode stream handler) (NAME bitstring))
      (bitstring-pattern-expand mode stream NAME
      	(bitstring-pattern ("secret" mode stream handler))))
    ; float
    ((_ ("secret" mode stream handler) (NAME BITS float) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS float
      	(bitstring-pattern ("secret" mode stream handler) rest ...)))
    ; bigendian
    ((_ ("secret" mode stream handler) (NAME BITS big) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS big
      	(bitstring-pattern ("secret" mode stream handler) rest ...)))
    ; littleendian
    ((_ ("secret" mode stream handler) (NAME BITS little) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS little
      	(bitstring-pattern ("secret" mode stream handler) rest ...)))
    ; bitstring
    ((_ ("secret" mode stream handler) (NAME BITS bitstring) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS bitstring
      	(bitstring-pattern ("secret" mode stream handler) rest ...)))
    ; rewrite by default to (NAME BITS big)
    ((_ ("secret" mode stream handler) (NAME BITS) rest ...)
      (bitstring-pattern ("secret" mode stream handler) (NAME BITS big) rest ...))
    ; rewrite immidiate value
    ((_ ("secret" mode stream handler) (NAME) rest ...)
      (case (value-type?? NAME)
      	((#\I)
      	  (bitstring-pattern ("secret" mode stream handler)
      	                     (NAME 8 big) rest ...))
      	((#\X)
      	  (bitstring-pattern ("secret" mode stream handler)
      	                     (NAME 8 big) rest ...))
      	((#\C)
      	  (bitstring-pattern ("secret" mode stream handler)
      	                     ((char->integer NAME) 8 big) rest ...))
      	((#\S)
      	  (let ((bits (* 8 (string-length NAME))))
      	    (bitstring-pattern ("secret" mode stream handler)
      	                       (NAME bits bitstring) rest ...)))
      	(else
      	  (error "bitstring-immidiate-value"))))
    ; dismiss other pattern forms
    ((_ ("secret" mode stream handler) NAME rest ...)
      (error "bitstring-malformed-pattern" `NAME))))

(define-syntax bitstring-pattern-expand
  (syntax-rules ()
    ((_ "constructing" stream name continuation)
      (and-let* ((tmp (bitstring-of-any name)))
      	(bitstring-append stream tmp)
      	continuation))
    ((_ "constructing" stream name bits type continuation)
      (and-let* ((tmp (bitstring-write-expand name bits type)))
      	(bitstring-append stream tmp)
      	continuation))
    ((_ "matching" stream name continuation) ; read all rest bytes
      (symbol?? name
      	(and-let* ((bits (bitstring-length stream))
      	           (name (bitstring-read stream bits)))
      	  continuation)
      	(abort (list 'bitstring-invalid-value `(name)))))
    ((_ "matching" stream name bits type continuation)
      (symbol?? name
      	(and-let* ((tmp (bitstring-read stream bits))
      	           (name (bitstring-read-expand tmp bits type)))
      	  ;(print "expand: " `(name bits type) " rest: " `continuation)      	  
      	  continuation)
      	(and-let* ((tmp (bitstring-read stream bits))
      	           (value (bitstring-write-expand name bits type)))
      	  (and
      	    (bitstring-compare tmp value)
      	    continuation))))))
    

(define-syntax bitstring-read-expand
  (syntax-rules (big little bitstring float)
    ((_ tmp bits big)
      (bitstring->integer-big tmp))
    ((_ tmp bits little)
      (bitstring->integer-little tmp))
    ((_ tmp bits bitstring)
      tmp) ; return bitstring as is
    ((_ tmp 16 float)
      (bitstring->half tmp))
    ((_ tmp 32 float)
      (bitstring->single tmp))))

(define-syntax bitstring-write-expand
  (syntax-rules (big little bitstring float)
    ((_ tmp bits big)
      (integer->bitstring-big tmp bits))
    ((_ tmp bits little)
      (integer->bitstring-little tmp bits))
    ((_ tmp bits bitstring)
      (if (bitstring? tmp)
      	tmp
      	(bitstring-of-any tmp)))
    ((_ tmp 16 float)
      (half->bitstring tmp))
    ((_ tmp 32 float)
      (single->bitstring tmp))))

;;;;;;;;;;;;;;;;;;;;;;
;; bitstring

(define-record bitstring
  offset  ; offset in bits
  numbits ; length of the bitstring in bits
  buffer  ; any container with random access
  getter  ; (lambda (buffer index) -> byte)
  setter) ; (lambda (buffer index byte) -> void)

(define-record-printer (bitstring x out)
  (fprintf out "<bitstring ~A ~A ~A>"
    (bitstring-offset x)
    (bitstring-numbits x)
    (bitstring->list x)))

(define (bitstring-length bs)
  (- (bitstring-numbits bs) (bitstring-offset bs)))
  
(define (bitstring-default-getter vec index)
  (u8vector-ref vec index))

(define (bitstring-default-setter vec index byte)
  (u8vector-set! vec index byte))

(define (bitstring-reserve numbits)
  (let* ((n (quotient numbits 8))
      	 (rem (remainder numbits 8))
    	 (aligned-size (if (zero? rem) n (+ 1 n))))
    (make-bitstring 0 numbits (make-u8vector aligned-size 0)
      bitstring-default-getter
      bitstring-default-setter)))

(define (bitstring-of-string s)
  (make-bitstring 0 (* 8 (string-length s)) s 
    (lambda (str index) (char->integer (string-ref str index)))
    (lambda (str index byte) (string-set! str index (integer->char byte)))))

(define (bitstring-of-vector v)
  (make-bitstring 0 (* 8 (vector-length v)) v
    (lambda (vec index) (vector-ref vec index))
    (lambda (vec index byte) (vector-set! vec index byte))))

(define (bitstring-of-u8vector v)
  (make-bitstring 0 (* 8 (u8vector-length v)) v
    bitstring-default-getter
    bitstring-default-setter))

(define (bitstring-of-any x)
  (cond
    ((bitstring? x)
      (bitstring-share x (bitstring-offset x) (bitstring-numbits x)))
    ((u8vector? x)
      (bitstring-of-u8vector x))
    ((string? x)
      (bitstring-of-string x))
    ((vector? x)
      (bitstring-of-vector x))))

(define (bitstring->list bs)
  (reverse
    (bitstring-fold 
      (lambda (offset n b acc)
      	(let ((byte (arithmetic-shift b (- n 8))))
      	  (append (list byte) acc)))
      '()
      bs)))

(define (bitstring-compare a b)
  (and
    ;(begin (print "bitstring-compare:" a b) #t)
    (= (bitstring-length a) (bitstring-length b))
    (equal? (bitstring->list a) (bitstring->list b))))

(define (bitstring-load-byte bitstring index)
  (let ((readb (bitstring-getter bitstring)))
    (readb (bitstring-buffer bitstring) index)))

(define (bitstring-load-word bitstring index)
  (bitwise-ior
    (arithmetic-shift (bitstring-load-byte bitstring index) 8)
    (bitstring-load-byte bitstring (+ index 1))))
 
(define (bitstring-store-byte bitstring index value)
  (let ((writeb (bitstring-setter bitstring)))
    (writeb (bitstring-buffer bitstring) index (bitwise-and #xFF value))))

(define (bitstring-fold func init-value bitstring)
  (let* ((offset (bitstring-offset bitstring))
         (count (bitstring-numbits bitstring))
         (shift (remainder offset 8)))
    (if (zero? shift)
      (bitstring-fold-aligned func init-value bitstring offset count)
      (bitstring-fold-shifted func init-value bitstring offset count shift))))        

(define (bitstring-fold-aligned func init-value bitstring from to)
  (let loop ((offset from)
             (index (quotient from 8))
             (acc init-value))
    (let ((n (min 8 (- to offset))))
      (if (<= n 0)
      	acc
      	(loop (+ offset n) (+ 1 index)
      	  (func offset n (bitstring-load-byte bitstring index) acc))))))

(define (bitstring-fold-shifted func init-value bitstring from to shift)
  (let loop ((offset from)
      	     (index (quotient from 8))
      	     (acc init-value))
    (let ((n (min 8 (- to offset))))
      (cond
      	((<= n 0)
      	  acc)
      	((< 8 (+ n shift))
      	  ; read splitted bits as word with shift
      	  (let* ((word (bitstring-load-word bitstring index))
      	         (drift (- shift 8))
      	         (byte (bitwise-and (arithmetic-shift word drift) #xFF)))
      	    (loop (+ offset n) (+ 1 index) (func offset n byte acc))))
      	(else ; read rest bits
      	  (let* ((tmp (bitstring-load-byte bitstring index))
      	         (drift (remainder offset 8))
      	         (byte (bitwise-and (arithmetic-shift tmp drift) #xFF)))
      	    ;(print (sprintf "tmp:~X n:~A dritf:~A byte:~X  shift:~A" tmp n drift byte shift)) 
      	    (loop (+ offset n) (+ 1 index) (func offset n byte acc))))))))
	
(define (integer-fold func init-value read-byte count)
  (let loop ((offset 0)
             (index 0)
             (acc init-value))
    (let ((n (min 8 (- count offset))))
      (cond 
      	((<= n 0)
      	  acc)
      	(else
      	  (loop (+ offset n) (+ 1 index)
      	    (func index n (read-byte offset n) acc)))))))

(define (bitstring->integer-little bitstring)
  (let ((start-offset (bitstring-offset bitstring)))
    (bitstring-fold
      (lambda (offset n b acc)
      	(let ((bits (arithmetic-shift b (- n 8)))
      	      (shift (- offset start-offset)))
      	  (bitwise-ior (arithmetic-shift bits shift) acc)))
      0
      bitstring)))

(define (bitstring->integer-big bitstring)
  (bitstring-fold
    (lambda (offset n b acc)
      (let ((bits (arithmetic-shift b (- n 8))))
      	(bitwise-ior (arithmetic-shift acc n) bits)))
    0
    bitstring))
    
(define (integer->bitstring-little value count)
  (integer-fold
    (lambda (index n b acc)
      (bitstring-store-byte acc index (arithmetic-shift b (- 8 n)))
      acc)
    (bitstring-reserve count)
    (lambda (offset n) 
      (bitwise-and (arithmetic-shift value (- offset)) #xFF))
    count))

(define (integer->bitstring-big value count)
  (integer-fold
    (lambda (index n b acc)
      (bitstring-store-byte acc index b)
      acc)
    (bitstring-reserve count)
    (lambda (offset n)
      (let* ((r (- count offset n))
      	     (b (arithmetic-shift value (- r))))
      	(arithmetic-shift (bitwise-and b #xFF) (- 8 n))))
    count))

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

(define (bitstring->single bs)
  (let ((s (bitstring-read bs 1))
        (e (bitstring-read bs 8))
        (m (bitstring-read bs 23)))
    (make-single-float
      (bitstring->integer-big s)
      (bitstring->integer-big e)
      (bitstring->integer-big m))))

(define (make-single-float signbit exponent mantissa)
  (cond
    ((and (zero? exponent) (zero? mantissa))
      (if (zero? signbit) +0. -0.))
    ((= exponent 255)
      (if (zero? mantissa)
      	(if (zero? signbit) +inf.0 -inf.0)
      	(if (zero? signbit) +nan.0 -nan.0)))
    (else
      (let ((e (- exponent 127))
      	    (m (bitwise-ior #x800000 mantissa)))
      	(let loop ((i 23) (s 1.) (f 0.))
      	  (let* ((x (arithmetic-shift 1 i))
      	         (b (bitwise-and m x)))
      	    (if (or (zero? i))
      	      (* f (expt 2 e) (if (zero? signbit) 1. -1.))
      	      (loop (- i 1) (/ s 2) (if (zero? b) f (+ f s))))))))))
      
(define (bitstring-share bs from to)
  (let ((numbits (bitstring-numbits bs)))
    (and
      (<= from to)
      (<= to numbits)
      (make-bitstring from to 
      	(bitstring-buffer bs)
      	(bitstring-getter bs)
      	(bitstring-setter bs)))))
   
(define (bitstring-read bs count)
  (let* ((from (bitstring-offset bs))
         (to (+ from count))
         (shared (bitstring-share bs from to)))
    (if shared
      (begin
      	(bitstring-offset-set! bs to)
      	shared)
      #f)))

; create empty bitstring and reserve 16 bytes
(define (bitstring-create)
  (let ((tmp (bitstring-of-u8vector (make-u8vector 16 0))))
    (bitstring-numbits-set! tmp 0)
    tmp))

(define (bitstring-buffer-size bs)
  (let ((buffer (bitstring-buffer bs)))
    (* 8 ; return size in bits
      (cond
      	((u8vector? buffer)
      	  (u8vector-length buffer))
      	((string? buffer)
      	  (string-length buffer))
      	(else
      	  (abort "not implemented for this buffer type"))))))

(define (bitstring-buffer-resize bs size-in-bits)
  (let* ((new-size (inexact->exact (/ size-in-bits 8)))
         (tmp (make-u8vector new-size 0))
         (used (bitstring-buffer-size bs)))
    (let copy ((i 0)
    	       (e (quotient used 8)))
      (when (< i e)
        (u8vector-set! tmp i (bitstring-load-byte bs i))
        (copy (+ i 1) e)))
    ; replace buffer with accessors
    (bitstring-buffer-set! bs tmp)
    (bitstring-setter-set! bs bitstring-default-setter)
    (bitstring-getter-set! bs bitstring-default-getter)))

(define (bitstring-append dest src)
  ; need ensure that dest buffer long enough
  (let ((required (bitstring-length src))
        (position (bitstring-numbits dest))
        (reserved (bitstring-buffer-size dest)))
    (when (< (- reserved position) required)
      (bitstring-buffer-resize dest
      	; grow buffer by 25% + required length
      	(+ reserved (* 0.25 reserved) required)))
    (bitstring-fold
      (lambda (offset nbits byte acc)
      	(bitstring-append-safe acc byte nbits))
      dest
      src)))

(define (bitstring-append-safe bs byte nbits)
  (let* ((position (bitstring-numbits bs))
         (index (quotient position 8))
         (drift (remainder position 8)))
    (if (zero? drift) 
      ; store aligned
      (begin
      	(bitstring-store-byte bs index byte)
      	(bitstring-numbits-set! bs (+ position nbits)))
      ; store unaligned
      (let ((byte-src (bitstring-load-byte bs index))
      	    (byte-dst (arithmetic-shift byte (- drift)))
      	    (restbits (- 8 drift)))
      	(bitstring-store-byte bs index (bitwise-ior byte-src byte-dst))
      	; store rest bits if didnt fit in current byte
      	(if (< restbits nbits)
      	  (bitstring-store-byte bs (+ index 1) (arithmetic-shift byte restbits)))
      	(bitstring-numbits-set! bs (+ position nbits))))
    bs));return bitstring

);module

