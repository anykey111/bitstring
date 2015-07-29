(use srfi-4 bitstring test)

(current-test-epsilon .01)

(test-begin "bitstring")

;;;;;;;;;;;;;;;;;;

(test-begin "bit-set?")
(test #t (bitstring-bit-set? (->bitstring '#${80 00}) 0))
(test #t (bitstring-bit-set? (->bitstring '#${01 00}) 7))
(test #t (bitstring-bit-set? (->bitstring '#${00 01}) -1))
(test #t (bitstring-bit-set? (->bitstring '#${80 00}) -16))
(test-end)

(test-begin "construct bitstring syntax")
(define foo "\x01")
(test (bitconstruct (1)(2)) (bitconstruct (foo bitstring) (2)))
(test (bitconstruct (1)) (bitconstruct (foo bitstring)))
(test-end)

(test-begin "integer attributes")
(test -25 (bitmatch "\xE7" ((x 8 signed) -> x)))
;(test -45 (bitmatch "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xD3" ((skip 1) (x 63 signed) -> x)))
(define bstr (->bitstring "\xFE"))
(test -2 (bitmatch bstr ((x signed) -> x)))
(test 254 (bitmatch bstr ((x unsigned) -> x)))
(test -2 (bitmatch bstr ((x 8 signed) -> x)))
(test 254 (bitmatch bstr ((x 8 unsigned) -> x)))
(test -2 (bitmatch bstr ((x 8 big signed) -> x)))
(test 254 (bitmatch bstr ((x 8 big unsigned) -> x)))
(test -2 (bitmatch bstr ((x 8 little signed) -> x)))
(test 254 (bitmatch bstr ((x 8 little unsigned) -> x)))
(test -2 (bitmatch bstr ((x 8 signed host) -> x)))
(test 254 (bitmatch bstr ((x 8 unsigned host) -> x)))
(test-error (bitmatch bstr ((x 8 unsigned cost) -> x)))
(test -1 (bitmatch (bitstring-share bstr 0 4) ((x 4 signed) -> x)))
(test -2 (bitmatch (bitstring-share bstr 4 8) ((x 4 signed) -> x)))
(test-end)

(test-begin "bitstring->list")
(define bstr (->bitstring "\xff"))
(test (make-list 8 1) (bitstring->list bstr 1 'big))
(test (make-list 8 1) (bitstring->list bstr 1 'little))
(test (make-list 8 1) (bitstring->list bstr 1 'host))
(test-end)

(test-begin "list->bitstring")
(define foo (list 1 0 1))
(define bar (list->bitstring foo 8))
(test foo (bitstring->list bar 8))
(test foo (bitstring->list bar 8 'big))
(test foo (bitstring->list bar 8 'little))
(test foo (bitstring->list bar 8 'host))
(test-end)

(test-begin "bitstring-reverse")
(define bs (->bitstring '#${0a 0b 0c 0d}))
(test (list #xd #xc #xb #xa) (bitstring->list (bitstring-reverse bs 8) 8))
(test-end)

(test-begin "bitstring <-> vector")
(define x (vector 1 2 3))
(test x (bitstring->vector (vector->bitstring x)))
(test-end)

(define bs9)
(define bs7)
(bitmatch (u8vector #xff #xff)
  (((a 9 bitstring) (b bitstring))
    (set! bs9 a)
    (set! bs7 b)))

(test-begin "bitstring <-> u8vector")
(define x (u8vector 1 2 3))
(test x (bitstring->u8vector (u8vector->bitstring x)))
(define y (u8vector #xff #x01))
(test y (bitstring->u8vector bs9))
(define z (u8vector #xff #x80))
(test z (bitstring->u8vector bs9 'right))
(define w (u8vector #b01111111))
(test w (bitstring->u8vector bs7 'left))
(define g (u8vector #b11111110))
(test g (bitstring->u8vector bs7 'right))
(test-end)

(test-begin "bitstring <-> blob")
(define x '#${1 2 3})
(test x (bitstring->blob (blob->bitstring x)))
(define y '#${ff01})
(test y (bitstring->blob bs9))
(define z '#${ff80})
(test z (bitstring->blob bs9 'right))
(test-end)

(test-begin "bitstring <-> string")
(define x "123")
(test x (bitstring->string (string->bitstring x)))
(test-end)

(test-begin "single-double")
(define a (bitconstruct (0.123 float)))
(define b (bitconstruct (0.2 double)))
(test 0.123 (bitmatch a (((x float)) x)))
(test 0.2 (bitmatch b (((x double)) x)))

(test 0.123
      (bitmatch (bitconstruct (0.123 float little))
        (((f float little)) f)))

(test 0.123
      (bitmatch (bitconstruct (0.123 float big))
        (((f float big)) f)))

(test 0.123
      (bitmatch (bitconstruct (0.123 double host))
        (((f double host)) f)))

(test (list 63 191 124 237 145 104 114 176)
      (bitstring->list (bitconstruct (0.123 double big)) 8))

(test (list 176 114 104 145 237 124 191 63)
      (bitstring->list (bitconstruct (0.123 double little)) 8))

(test-end)

(test-begin "string-constant")
(test 2 (bitmatch "123" ((("234")) 1) ((("123")) 2)))
(define s123 "123")
(test 2 (bitmatch s123 ((("234")) 1) ((("123")) 2)))
(test 2 (bitmatch s123 ((("234")) 1) (((s123 bitstring)) 2)))
(test 2 (bitmatch "123" ((("234")) 1) (((s123 bitstring)) 2)))
(test-end)

(test-begin "construct")
(bitpacket NString (size 8) (data (* 8 size) bitstring))
(define (make-nstr str)
  (let ((size (string-length str))
        (data str))
    (bitconstruct (NString bitpacket))))
(define nstr (make-nstr "ABC"))
(test #t (bitmatch nstr (((3) ("ABC")) #t) (else #f)))
(test-end)

(test-begin "append")
; append list immutable
(test "1234567890"
 (bitstring->string
   (bitstring-append (->bitstring "123") (->bitstring "456") (->bitstring "7890"))))
; append list mutable
(define bs (->bitstring (u8vector)))
(bitstring-append! bs (->bitstring "123") (->bitstring "456") (->bitstring "7890"))
(test "1234567890" (bitstring->string bs))
; append aligned
(define bs (->bitstring (u8vector)))
(bitstring-append! bs (->bitstring "A"))
(bitstring-append! bs (->bitstring "B"))
(bitstring-append! bs (->bitstring "\x20"))
(test #t (bitstring=? bs (->bitstring "AB\x20")))
; test immutable append
(define a (->bitstring "A"))
(define b (->bitstring "B"))
(define c (bitstring-append a b))
(test #t (bitstring=? (bitconstruct ("AB")) c))
(test #t (bitstring=? (bitconstruct ("A")) a))
(test #t (bitstring=? (bitconstruct ("B")) b))
(test 16 (bitstring-length c))
; append unaligned
(define bs (->bitstring (u8vector)))
(bitstring-append! bs (integer->bitstring-big #b100 3))
(bitstring-append! bs (integer->bitstring-big #b10 2))
(bitstring-append! bs (integer->bitstring-big #b1 1))
(bitstring-append! bs (integer->bitstring-big #b0101 4))
(bitstring-append! bs (integer->bitstring-big #b10 2))
(bitstring-append! bs (integer->bitstring-big #b0 1))
(bitstring-append! bs (integer->bitstring-big #b10100 5))
(test #b100101010110010100 (bitstring->integer-big bs))
; append unaligned with overflow
(define bs (->bitstring (u8vector)))
(bitstring-append! bs (integer->bitstring-big #b100111010 9))
(bitstring-append! bs (integer->bitstring-big #b1000111100100 13))
(test #b1001110101000111100100 (bitstring->integer-big bs))
(define bs (->bitstring (u8vector)))
(bitstring-append! bs (integer->bitstring-big #b0 1))
(bitstring-append! bs (integer->bitstring-big #b01001011011101 14))
(bitstring-append! bs (integer->bitstring-big #b110001 6))
(bitstring-append! bs (integer->bitstring-big #b10100011100 11))
(test #b00100101101110111000110100011100 (bitstring->integer-big bs))
; append with resize
(define bs (->bitstring (u8vector)))
(let ((a "Is There Love")
      (b "in Space?")
      (c "Nobody knows."))
  (bitstring-append! bs (->bitstring a))
  (bitstring-append! bs (->bitstring b))
  (test #t (bitstring=? (->bitstring (string-append a b)) bs))
  (bitstring-append! bs (->bitstring c))
  (test #t (bitstring=? (->bitstring (string-append a b c)) bs)))
(test-end)

(test-begin "bitpacket")
(bitpacket Packet1 (1) (2))
(bitpacket Packet2 (A 8) (B))
(test 3 (bitmatch `#(1 2 3) (((Packet1 bitpacket) (C 8)) C)))
(test 6 (bitmatch `#(1 2 3) (((Packet2 bitpacket) (C 8)) (+ A B C))))
(test-error (bitmatch `#(1 2 3) (((Packet1 bitpacket) (C 8) (D 8)) C)))

(bitpacket PacketC (C 8))
(bitpacket PacketB (B 8))
(bitpacket PacketA (A 8) (PacketB bitpacket) (PacketC bitpacket))
(test 6 (bitmatch `#(1 2 3) (( (PacketA bitpacket) ) (+ A B C))))

(bitpacket PacketX (22) (ValueX 8))
(bitpacket PacketY (33) (ValueY 8))
(bitpacket PacketZ (44) (ValueZ 8))
(test 13 (bitmatch `#( 44 10 )
    (((PacketX bitpacket)) (+ 1 ValueX))
    (((PacketY bitpacket)) (+ 2 ValueY))
    (((PacketZ bitpacket)) (+ 3 ValueZ))))

;;bitpacket with prefix
(bitpacket Point (x 8) (y 8))

(test 5 (bitmatch "\x01\x02\x03\x04"
          (((p1 Point bitpacket) (p2 Point bitpacket))
           (+ p1.x p2.y))))

(bitpacket Line (start Point bitpacket)
                (end Point bitpacket))

(test 5 (bitmatch "\x01\x02\x03\x04"
          (((Line bitpacket))
           (+ start.x end.y))))

(test 5 (bitmatch "\x01\x02\x03\x04"
          (((line Line bitpacket))
           (+ line.start.x line.end.y))))

(test "\x01\x02\x03\x04" (let ((line.start.x 1)
                               (line.start.y 2)
                               (line.end.x 3)
                               (line.end.y 4))
                           (bitstring->string (bitconstruct (line Line bitpacket)))))
; bitpacket constructor
(bitpacket (Point3D make-Point3D)
  (x float host)
  (y float host)
  (z float host))
(test (f32vector 0.0 -1.0 1.0)
      (blob->f32vector (bitstring->blob (make-Point3D (x 0.0) (y -1.0) (z 1.0)))))

(test-end)

(test-begin "->bitstring")
(test 'ok (bitmatch "ABC" ((("A") (66) (#\C)) 'ok)))
(test 'ok (bitmatch "ABC" ((("AB") (#\C)) 'ok)))
(test 'ok (bitmatch `#( 65 66 67 ) ( (("A") (66) (#\C)) 'ok)))
(test 'ok (bitmatch `#u8( 65 66 67 ) ((("A") (66) (#\C)) 'ok)))
(test 'ok (bitmatch (string->blob "ABC") ((("A") (66) (#\C)) 'ok)))
(test-error (bitmatch (s8vector 65 66 67) ((("A") (66) (#\C)) 'ok)))

(bitmatch `#( 5 1 2 3 4 5)
  (((count 8) (rest (* count 8) bitstring))
    (print " count=" count " rest=" (bitstring-length rest))))
(test-end)

(test-begin "short form")
(bitpacket B30 (30))
(test 'yes (bitmatch `#( 10 20 30 )
    (((10) (20) (11)) 'no)
    (((10) (20) (33)) 'no)
    (((10) (20) (B30 bitpacket)) 'yes)))
(test-end)

(test-begin "match")

#;(test 1.5
  (bitmatch `#( #x38 #x00  #x00 #x00 #x80 #x3f)
    (((a 16 float) (b 32 float))
      (+ a b))))

(test (list 1 15)
  (bitmatch `#( #x8F )
    (((flagBit 1 big) (restValue 7)) (list flagBit restValue))))

(test 'ok
  (bitmatch `#( #x8F )
    (((1 1) (rest)) 'fail)
    (((x 1) (check (= x 0)) (rest bitstring)) 'fail2) 
    (((1 1) (rest bitstring)) 'ok)))

(test 'ok
  (bitmatch `#( #x8F )
    (((#x8E)) 'fail1)
    (((#x8C)) 'fail2)
    (((#x8F)) 'ok)))

(test 'ok
  (bitmatch `#( #x8F )
    (((#x8E)) 'fail1)
    (((#x8C)) 'fail2)
    (else 'ok)))

(test-end)

(test-begin "read")
(define bs (vector->bitstring `#(65 66 67)))
(test #f (bitstring-read bs 100))
(test 2 (bitstring->integer-big (bitstring-share bs 0 3)))
(test 5 (bitstring->integer-big (bitstring-share bs 3 10)))
(test 579 (bitstring->integer-big (bitstring-share bs 10 24)))
(test 2 (bitstring->integer-big (bitstring-read bs 3)))
(test 5 (bitstring->integer-big (bitstring-read bs 7)))
(test 579 (bitstring->integer-big (bitstring-read bs 14)))
(test #f (bitstring-read bs 1))
(define bs (vector->bitstring `#( #x8F )))
(test 1 (bitstring->integer-big (bitstring-share bs 0 1)))
(test 15 (bitstring->integer-big (bitstring-share bs 1 8)))
(define bs (vector->bitstring `#( #x7C #x00)))
(test 0 (bitstring->integer-big (bitstring-share bs 0 1)))
(test 31 (bitstring->integer-big (bitstring-share bs 1 6)))
(test-end)

(define (get-fields bs)
  (list (bitstring-start bs) (bitstring-end bs) (bitstring-buffer bs)))

(test-begin "big")
(test `(0 0 #u8()) (get-fields (integer->bitstring-big 0 0)))
(test `(0 3 #u8(32)) (get-fields (integer->bitstring-big 1 3)))
(test 1 (bitstring->integer-big (integer->bitstring-big 1 3)))
(test `(0 8 #u8(15)) (get-fields (integer->bitstring-big 15 8)))
(test 15 (bitstring->integer-big (integer->bitstring-big 15 8)))
(test `(0 9 #u8(94 0)) (get-fields (integer->bitstring-big #xABC 9)))
(test 188 (bitstring->integer-big (integer->bitstring-big #xABC 9)))
(test `(0 10 #u8(175 0)) (get-fields (integer->bitstring-big #xABC 10)))
(test 700 (bitstring->integer-big (integer->bitstring-big #xABC 10)))
(test 123213 (bitstring->integer-big (integer->bitstring-big 123213 32)))
(test #x00000001 (bitstring->integer-big (integer->bitstring-big #x00000001 32)))
(test #x10000000 (bitstring->integer-big (integer->bitstring-big #x10000000 32)))
(test #x7FFFFFFF (bitstring->integer-big (integer->bitstring-big #x7FFFFFFF 32)))
(test #xFFFFFFFF (bitstring->integer-big (integer->bitstring-big #xFFFFFFFF 32)))
(test-end)

(test-begin "little")
(test `(0 0 #u8()) (get-fields (integer->bitstring-little 0 0)))
(test `(0 3 #u8(32)) (get-fields (integer->bitstring-little 1 3)))
(test 1 (bitstring->integer-little (integer->bitstring-little 1 3)))
(test `(0 8 #u8(15)) (get-fields (integer->bitstring-little 15 8)))
(test 15 (bitstring->integer-little (integer->bitstring-little 15 8)))
(test `(0 9 #u8(188 0)) (get-fields(integer->bitstring-little #xABC 9)))
(test 188 (bitstring->integer-little (integer->bitstring-little #xABC 9)))
(test `(0 10 #u8(188 128)) (get-fields (integer->bitstring-little #xABC 10)))
(test 700 (bitstring->integer-little (integer->bitstring-little #xABC 10)))
(test 123213 (bitstring->integer-little (integer->bitstring-little 123213 32)))
(test #x00000001 (bitstring->integer-little (integer->bitstring-little #x00000001 32)))
(test #x10000000 (bitstring->integer-little (integer->bitstring-little #x10000000 32)))
(test #x7FFFFFFF (bitstring->integer-little (integer->bitstring-little #x7FFFFFFF 32)))
(test #xFFFFFFFF (bitstring->integer-little (integer->bitstring-little #xFFFFFFFF 32)))
(test-end)

(test-begin "half")
(test +inf.0 (bitstring->half (vector->bitstring `#( #x7C #x00))))
(test -inf.0 (bitstring->half (vector->bitstring `#( #xFC #x00))))
(test 0. (bitstring->half (vector->bitstring `#( #x00 #x00))))
(test -0. (bitstring->half (vector->bitstring `#( #x80 #x00))))
(test 0.5 (bitstring->half (vector->bitstring `#( #x38 #x00))))
(test 1. (bitstring->half (vector->bitstring `#( #x3C #x00))))
(test 25. (bitstring->half (vector->bitstring `#( #x4E #x40))))
(test 0.099976 (bitstring->half (vector->bitstring `#( #x2E #x66))))
(test -0.122986 (bitstring->half (vector->bitstring `#( #xAF #xDF))))
;-124.0625
(test-end)

(test-begin "single")
(test +inf.0 (bitstring->single (vector->bitstring `#( #x00 #x00 #x80 #x7F))))
(test -inf.0 (bitstring->single (vector->bitstring `#( #x00 #x00 #x80 #xFF))))
;(test +nan.0 (bitstring->single (vector->bitstring `#( #x7F #xC0 #x00 #x00))))
(test 0. (bitstring->single (vector->bitstring `#( #x00 #x00 #x00 #x00))))
(test -0. (bitstring->single (vector->bitstring `#( #x00 #x00 #x00 #x80))))
(test #t (equal? 1. (bitstring->single (vector->bitstring `#( #x00 #x00 #x80 #x3f)))))
(test 0.5 (bitstring->single (vector->bitstring `#( #x00 #x00 #x00 #x3f))))
(test 25. (bitstring->single (vector->bitstring `#( #x00 #x00 #xc8 #x41))))
(test 0.1 (bitstring->single (vector->bitstring `#( #xcd #xcc #xcc #x3d))))
(test -0.123 (bitstring->single (vector->bitstring `#( #xE7 #x6D #xFB #xBD))))
(test `(0 32 #u8( #x00 #x00 #x00 #x3f)) (get-fields (single->bitstring 0.5)))
(test `(0 32 #u8( #x6D #xE7 #xFB #xBD)) (get-fields (single->bitstring -0.123)))
(test-end)

(test-begin "boolean")
(test #t (bitmatch (bitconstruct (#t boolean))
           (((B boolean)) B)))

(test #t (bitmatch (bitconstruct (#t 32 boolean))
           (((B 32 boolean)) B)))

(test #t (bitmatch (bitconstruct (#t 32 boolean little))
           (((B 32 boolean little)) B)))

(test #f (bitmatch (bitconstruct (#f 16 boolean host))
           (((B 16 boolean host)) B)
           (((X bitstring)) X)))

(test (vector 0.0 0.0 #t #t)
  (bitmatch (u8vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)
    (((a double big)  (b double big) (c boolean) (d boolean))
     (vector a b c d))))

(test (list 1) (bitstring->list (bitconstruct (#t boolean)) 8))
(test (list 0) (bitstring->list (bitconstruct (#f boolean)) 8))
(test-end)

(test-begin "reader proc")

(define (zterminated-string bs)
  (let loop ((n 8) (acc '()))
    (bitmatch (or (bitstring-read bs 8) "")
      (() -> #f) ; end of stream
      ((0) -> (list n (list->string (reverse acc))))
      ((c) -> (loop (+ n 8) (cons (integer->char c) acc))))))
(test "BC" (bitmatch "ABC\x00" ((("A") ((str zterminated-string) bitstring)) str)))
(test-error (bitmatch "ABC" ((("A") ((str zterminated-string) bitstring)) str)))
(test #f (bitmatch "ABC" ((("A") ((str zterminated-string) bitstring)) str) (else #f)))

(test-end)

(test-end "bitstring")

(test-exit)

