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
   bitstring=?
   bitstring-append 
   bitstring-create
   bitstring->list
   bitstring->blob
   bitstring->integer
   bitstring->integer-big
   bitstring->integer-little
   bitstring->integer-host
   integer->bitstring-big
   integer->bitstring-little
   integer->bitstring-host
   bitstring-offset
   bitstring-numbits
   bitstring-buffer
   bitstring-getter
   bitstring->half
   bitstring->single
   single->bitstring
   bitstring->double
   double->bitstring
   bytestring?
   bytestring-fold
   )

  (import scheme chicken extras foreign)
  (require-extension srfi-1 srfi-4)

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

(define-syntax bitpacket
  (syntax-rules ()
    ((_ name fields ...)
      (define-syntax name
      	(er-macro-transformer
      	  ;; (name (mode stream handler) args ...)
      	  (lambda (e r c)
      	    (let* ((context (cadr e))
                   (mode (first context))
                   (stream (second context))
                   (handler (third context))
      	    	     (args (cddr e)))
      	      ;; inline packet fields
              ;(print "inline:" mode stream handler " fields:" `(fields ...) " args:" args)
      	      `(bitstring-pattern-continue ,mode ,stream ,handler (fields ...) ,args)))
                  
      	  )))))

(define-syntax bitstring-pattern-continue
  (syntax-rules ()
    ((_ mode stream handler (fields ...) (rest ...))
      (bitstring-pattern mode stream handler fields ... rest ...))))

(define-syntax capture-handler
  (syntax-rules ()
    ((_ (handler ...))
      (lambda () handler ...))))

(define-syntax bitconstruct
  (syntax-rules ()
    ((_ patterns ...)
      (let ((bstr (bitstring-create)))
        (bitstring-pattern "write" bstr "no-handler" patterns ...)))))

(define-syntax bitmatch
  (syntax-rules ()
    ((_ value patterns ...)
      ;; invoke user code with captured variables
      ((let ((bstr (bitstring-of-any value)))
        (or (bitmatch-pattern-list bstr patterns ...)))))))

(define-syntax bitmatch-pattern-list
  (syntax-rules (else ->)
    ((_ bstr (else handler ...))
      (capture-handler (handler ...)))
    ((_ bstr (pattern ... -> handler) rest ...)
      (bitmatch-pattern-list bstr ((pattern ...) handler) rest ...))
    ((_ bstr (pattern ... -> handler ...) rest ...)
      (bitmatch-pattern-list bstr ((pattern ...) handler ...) rest ...))
    ((_ bstr ((pattern ...) handler ...))
      (or
        (bitmatch-pattern bstr (handler ...) pattern ...)
        (error 'bitstring-match-failure)))
    ((_ bstr ((pattern ...) handler ...) rest ...)
      (or
        (bitmatch-pattern bstr (handler ...) pattern ...)
        (bitmatch-pattern-list bstr rest ...)))))

(define-syntax bitmatch-pattern
  (syntax-rules ()
    ((_ bstr handler pattern ...)
      ; share bitstring instance
      (let ((stream (bitstring-of-any bstr)))
        (bitstring-pattern "read" stream handler pattern ...)))))

(define-syntax bitstring-pattern
  (syntax-rules (big little host bitstring check float double bitpacket signed unsigned)
    ; all patterns take expansion
    ((_ "read" stream handler)
      (and
        ; ensure that no more bits left
        (zero? (bitstring-length stream))
        (capture-handler handler)))
    ((_ "write" stream handler)
      stream)
    ; zero-length bitstring
    ((_ "read" stream handler ())
      (and
        (zero? (bitstring-length stream))
        (capture-handler handler)))
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
    ; bitpacket
    ((_ mode stream handler (NAME bitpacket) rest ...)
      (bitstring-packet-expand mode stream handler NAME rest ...))
    ; bitpacket at tail
    ((_ mode stream handler (NAME bitpacket))
      (bitstring-packet-expand mode stream handler NAME))
    ; allow in bitconstruct dont type length
    ((_ "write" stream handler (NAME bitstring) rest ...)
      (bitstring-pattern-expand "write" stream NAME
        (bitstring-pattern "write" stream handler rest ...)))
    ; greedy bitstring
    ((_ mode stream handler (NAME bitstring))
      (bitstring-pattern-expand mode stream NAME
        (bitstring-pattern mode stream handler)))
    ; double 64
    ((_ mode stream handler (NAME double) rest ...)
      (bitstring-pattern-expand mode stream NAME 64 float
        (bitstring-pattern mode stream handler rest ...)))
    ; float 32
    ((_ mode stream handler (NAME float) rest ...)
      (bitstring-pattern-expand mode stream NAME 32 float
        (bitstring-pattern mode stream handler rest ...)))
    ; float bits
    ((_ mode stream handler (NAME BITS float) rest ...)
      (bitstring-pattern-expand mode stream NAME BITS float
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
      (error "bitstring-malformed-pattern" `mode `stream `handler `rest))))

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

(define-syntax bitstring-packet-expand
  (syntax-rules ()
    ((_ mode stream handler name)
      (name (mode stream handler)))
    ((_ mode stream handler name rest ...)
      (name (mode stream handler) rest ...))))

(define-syntax bitstring-pattern-expand
  (syntax-rules ()
    ((_ "write" stream name continuation)
      (and-let* ((tmp (bitstring-of-any name)))
        ;(print "write-expand:" `stream " name:" `name)
      	(bitstring-append stream tmp)
      	continuation))
    ((_ "write" stream name bits type continuation)
      (and-let* ((tmp (bitstring-write-expand name bits type)))
        ;(print "write-expand:" `stream " name:" `name)
      	(bitstring-append stream tmp)
      	continuation))
    ((_ "read" stream name continuation) ; read all rest bytes
      (symbol?? name
      	(and-let* ((bits (bitstring-length stream))
      	           (name (bitstring-read stream bits)))
          ;(print "read-expand: " `(name bits type) " rest: " `continuation)         
      	  continuation)
      	(abort (list 'bitstring-invalid-value `(name)))))
    ((_ "read" stream name bits type continuation)
      (symbol?? name
      	(and-let* ((tmp (bitstring-read stream bits))
      	           (name (bitstring-read-expand tmp bits type)))
      	  ;(print "expand-symbol: " `(name bits type) " rest: " `continuation)      	  
      	  continuation)
      	(and-let* ((tmp (bitstring-read stream bits))
      	           (value (bitstring-write-expand name bits type)))
          ;(print "expand-value: " `(name bits type) " rest: " `continuation)
      	  (and
      	    (bitstring=? tmp value)
      	    continuation))))))

(define-syntax bitstring-read-expand
  (syntax-rules (bitstring float)
    ((_ tmp bits (ENDIAN SIGNED))
      (bitstring-read-integer tmp bits ENDIAN SIGNED))
    ((_ tmp bits bitstring)
      tmp) ; return bitstring as is
    ((_ tmp 16 float)
      (bitstring->half tmp))
    ((_ tmp 32 float)
      (bitstring->single tmp))
    ((_ tmp 64 float)
      (bitstring->double tmp))))

(define-syntax integer->signed
  (syntax-rules ()
    ((_ BITS VALUE)
      (let ((SBIT-INDEX (- BITS 1)))
        (if (bit-set? VALUE SBIT-INDEX)
          (- (bitwise-xor (arithmetic-shift 1 SBIT-INDEX) VALUE))
          VALUE)))))

(define-syntax bitstring-read-integer
  (syntax-rules (big little host signed unsigned)
    ((_ tmp bits big signed)
      (integer->signed bits (bitstring->integer-big tmp)))
    ((_ tmp bits little signed)
      (integer->signed bits (bitstring->integer-little tmp)))
    ((_ tmp bits host signed)
      (integer->signed bits (bitstring->integer-host tmp)))
    ((_ tmp bits big unsigned)
      (bitstring->integer-big tmp))
    ((_ tmp bits little unsigned)
      (bitstring->integer-little tmp))
    ((_ tmp bits host unsigned)
      (bitstring->integer-host tmp))
    ((_ tmp bits ENDIAN SIGNED)
      (error "invalid integer attibute" `ENDIAN `SIGNED))))

(define-syntax bitstring-write-expand
  (syntax-rules (bitstring float)
    ((_ tmp bits (ENDIAN SIGNED))
      (bitstring-write-integer tmp bits ENDIAN SIGNED))
    ((_ tmp bits bitstring)
      (if (bitstring? tmp)
      	tmp
      	(bitstring-of-any tmp)))
    ((_ tmp 16 float)
      (half->bitstring tmp))
    ((_ tmp 32 float)
      (single->bitstring tmp))
    ((_ tmp 64 float)
      (double->bitstring tmp))))

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
      (error "invalid integer attibute" `ENDIAN `SIGNED))))


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

(define (bitstring-fold-bytes fun initial bs)
  (bitstring-fold 
      (lambda (offset n b acc)
        (let ((byte (arithmetic-shift b (- n 8))))
          (fun byte acc)))
        initial
        bs))

(define (bitstring->blob bs)
  ;NOTE: optimize me! 
  (u8vector->blob (list->u8vector (bitstring->list bs 8))))
    
(define (bitstring->list bs #!optional (bits 1) (endian 'big))
  (if (= bits 8)
    (bitstring->list8 bs)
    (bitstring->listn bs bits endian)))

(define (bitstring->list8 bs)
  (reverse
    (bitstring-fold-bytes
      (lambda (byte acc)
        (cons byte acc))
        (list)
        bs)))

(define (bitstring->listn bs bits endian)
  (let loop ((data bs)
             (acc (list)))
    (bitmatch data
      (()
        (reverse acc))
      (((value bits bitstring) (rest bitstring))
        (loop rest
          (cons (bitstring->integer value endian)
                acc)))
      (((rest-value bitstring))
        (loop (bitstring-of-any "")
          (cons (bitstring->integer rest-value endian)
                acc))))))

(define (bitstring=? a b)
  (and
    ;(begin (print "bitstring-compare:" a b) #t)
    (= (bitstring-length a) (bitstring-length b))
    (equal? (bitstring->list a 8) (bitstring->list b 8))))

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

(define bitstring->integer-host
  (cond-expand
    (little-endian bitstring->integer-little)
    (else bitstring->integer-big)))
    
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

(define integer->bitstring-host
  (cond-expand
    (little-endian integer->bitstring-little)
    (else integer->bitstring-big)))

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

(define float->uint32
  (foreign-lambda* void ((u8vector i) (float f))
    "*(uint32_t*)i = *(uint32_t*)&f;"))

(define double->uint64
  (foreign-lambda* void ((u8vector i) (double d))
    "*(uint64_t*)i = *(uint64_t*)&d;"))

(define uint32->float
  (foreign-lambda* float ((blob i))
    "C_return(*(float*)i);"))

(define uint64->double
  (foreign-lambda* double ((blob i))
    "C_return(*(double*)i);"))
    
(define (single->bitstring value)
    (let ((buf (make-u8vector 4)))
        (float->uint32 buf value)
        (bitstring-of-any buf)))

(define (double->bitstring value)
    (let ((buf (make-u8vector 8)))
        (double->uint64 buf value)
        (bitstring-of-any buf)))

(define (bitstring->single bs)
    (uint32->float (bitstring->blob bs)))

(define (bitstring->double bs)
    (uint64->double (bitstring->blob bs)))
            
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
  (let* ((new-size (inexact->exact (ceiling (/ size-in-bits 8))))
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

(define (bytestring? bs)
  (and (zero? (remainder (bitstring-offset bs) 8))
       (zero? (remainder (bitstring-length bs) 8))))

(define (bytestring-fold proc init-value bs)
  (or (bytestring? bs)
      (error "bytestring shouldbe 8 bit aligned bitstring"))
  (let ((size (fx/ (bitstring-length bs) 8))
        (read-byte (bitstring-getter bs))
        (buffer (bitstring-buffer bs)))
    (let loop ((index 0)
               (acc init-value))
      (if (< index size)
        (loop (+ index 1) (proc (read-byte buffer index) acc))
        acc))))

);module
