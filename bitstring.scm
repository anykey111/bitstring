;; bitstirng module implements the subset of Erlang bit syntax.
;; 
;; Basic syntax description
;; (bitmatch vector-or-bitstring 
;;   (pattern1 .. patternN -> expression)
;;   ...
;;   (else expression))
;;
;; Notes:
;; - else block is optional.
;; - default endianes bigendian.
;; - default count 8 bits.
;; - if nothing matches and else guard didnt specified 'bitstring-match-failure
;; exception will thrown.
;;
;; Variable binding:
;; ( let NAME )
;; ( let NAME bitstring)
;; ( let NAME count )
;; ( let NAME count big)
;; ( let NAME count little)
;;
;; Compare with integer value:
;; ( value )
;; ( value count )
;; ( value count big )
;; ( value count little )
;; 
;; Guard condition, continue evaluate only when expression returns #t.
;; (check expression)
;;

(module bitstring
  (bitmatch
   make-bitstring
   bitstring-length
   bitstring-of-any
   bitstring-of-vector
   bitstring-read
   bitstring-share
   bitstring-compare
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

(define-syntax bitmatch
  (syntax-rules ()
    ((_ value patterns ...)
      (call-with-current-continuation
	(lambda (k)
	  (or (bitmatch2 ('secret-params value k) patterns ...)))))))

(define-syntax bitmatch2
  (syntax-rules (else) 
    ((_ ('secret-params value k))
      (abort (list 'bitstring-match-failure)))
    ((_ ('secret-params value k) (else expression))
      (k expression))
    ((_ ('secret-params value k) (pattern ...) rest ...)
      (or
      	(let ((stream (bitstring-of-any value)))
      	  (bitmatch1 ('secret-params stream k) pattern ...))
      	(bitmatch2 ('secret-params value k) rest ...)))))

(define-syntax bitmatch1
  ; CAUTION: use ##core#let during macro expansion !!!
  (syntax-rules (let big little bitstring check float -> )
    ; user guard expression
    ((_ ('secret-params stream k) (check condition) rest ...)
      (and
      	condition
      	(bitmatch1 ('secret-params stream k) rest ...)))
    ; read 16 bit float
    ((_ ('secret-params stream k) (let name 16 float) rest ...)
      (and-let* ((tmp (bitstring-read stream 16))
      	         (name (bitstring->half tmp)))
      	(bitmatch1 ('secret-params stream k) rest ...)))
    ; read 32 bit float
    ((_ ('secret-params stream k) (let name 32 float) rest ...)
      (and-let* ((tmp (bitstring-read stream 32))
      	         (name (bitstring->single tmp)))
      	(bitmatch1 ('secret-params stream k) rest ...)))
    ; rewrite => (let name 8 big) 
    ((_ ('secret-params stream k) (let name big) rest ...)
      (bitmatch1 ('secret-params stream k) (let name 8 big) rest ...))
    ; rewrite => (let name 8 little)
    ((_ ('secret-params stream k) (let name little) rest ...)
      (bitmatch1 ('secret-params stream k) (let name 8 little) rest ...))
    ; match rest value
    ((_ ('secret-params stream k) (let name bitstring) -> rest ...)
      (and-let* ((count (bitstring-length stream))
      	         (name (bitstring-read stream count)))
      	(bitmatch1 ('secret-params stream k) -> rest ...)))
    ; rewrite => (let name count big)
    ((_ ('secret-params stream k) (let name count) rest ...)
      (bitmatch1 ('secret-params stream k) (let name count big) rest ...))
    ; bind named integer
    ((_ ('secret-params stream k) (let name count big) rest ...)
      (and-let* ((tmp (bitstring-read stream count))
      	         (name (bitstring->integer-big tmp)))
      	(bitmatch1 ('secret-params stream k) rest ...)))
    ((_ ('secret-params stream k) (let name count little) rest ...)
      (and-let* ((tmp (bitstring-read stream count))
      	         (name (bitstring->integer-little tmp)))
      	(bitmatch1 ('secret-params stream k) rest ...)))
    ; bind named bitstring
    ((_ ('secret-params stream k) (let name count bitstring) rest ...)
      (and-let* ((name (bitstring-read stream count)))
      	(bitmatch1 ('secret-params stream k) rest ...)))
    ; rewrite => (let name 8 big)
    ((_ ('secret-params stream k) (let name) rest ...)
      (bitmatch1 ('secret-params stream k) (let name 8 big) rest ...))      
    ; compare bigendian values
    ((_ ('secret-params stream k) (value count big) rest ...)
      (and-let* ((tmp (bitstring-read stream count))
      	         (value2 (bitstring->integer-big tmp)))
      	(and
      	  (equal? value value2) 
      	  (bitmatch1 ('secret-params stream k) rest ...))))
    ; compare littleendian values
    ((_ ('secret-params stream k) (value count little) rest ...)
      (and-let* ((tmp (bitstring-read stream count))
      	         (value2 (bitstring->integer-little tmp)))
      	(and
      	  (equal? value value2) 
      	  (bitmatch1 ('secret-params stream k) rest ...))))
    ; rewrtie to => (value 8 big) 
    ((_ ('secret-params stream k) (value count) rest ...)
      (bitmatch1 ('secret-params stream k) (value count big) rest ...))
    ; compare 8 bit integer or bitstring 
    ((_ ('secret-params stream k) (value) rest ...)
      (cond
      	((integer? value)
      	  (bitmatch1 ('secret-params stream k) (value 8 big) rest ...))
      	((char? value)
      	  (bitmatch1 ('secret-params stream k) ((char->integer value) 8 big) rest ...))
      	(else
      	  (and-let* ((tmp1 (bitstring-of-any value))
      	             (tmp2 (bitstring-read stream (bitstring-length tmp1))))
      	    (and
      	      (bitstring-compare tmp1 tmp2)
      	      (bitmatch1 ('secret-params stream k) rest ...))))))
    ; user expression
    ((_ ('secret-params stream k) -> expression)
      (and
      	(zero? (bitstring-length stream))
      	(k expression)))
    ; empty expression body
    ((_ ('secret-params stream k))
      (abort (list 'bitstring-empty-pattern)))
    ((_ ('secret-params stream k) err1)
      (abort (list 'bitstring-malformed-pattern `err1)))
    ; unrecognized value
    ((_ ('secret-params stream k) err1 rest ...)
      (abort (list 'bitstring-malformed-pattern `err1)))))

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
    (bitstring-buffer x)))

(define (bitstring-length bs)
  (- (bitstring-numbits bs) (bitstring-offset bs)))
  
(define (bitstring-reserve numbits)
  (let* ((n (quotient numbits 8))
      	 (rem (remainder numbits 8))
    	 (aligned-size (if (zero? rem) n (+ 1 n))))
    (make-bitstring 0 numbits (make-u8vector aligned-size 0)
      (lambda (vec index) (u8vector-ref vec index))
      (lambda (vec index byte) (u8vector-set! vec index byte)))))

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
    (lambda (vec index) (u8vector-ref vec index))
    (lambda (vec index byte) (u8vector-set! vec index byte))))

(define (bitstring-of-any x)
  (cond
    ((bitstring? x)
      x) ;FIXME: make copy of x
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
  (bitstring-fold
    (lambda (offset n b acc)
      (let ((bits (arithmetic-shift b (- n 8))))
      	(bitwise-ior (arithmetic-shift bits offset) acc)))
    0
    bitstring))

(define (bitstring->integer-big bitstring)
  (bitstring-fold
    (lambda (offset n b acc)
      (let ((bits (arithmetic-shift b (- n 8)))
      	    (count (bitstring-numbits bitstring)))
      	;(print "offset: " offset (sprintf " b: ~X" b) " n: " n)
      	(bitwise-ior (arithmetic-shift bits (- count offset n)) acc)))
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

);module

