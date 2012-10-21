(load "bitstring.scm")

(use bitstring test)

(bitpacket Packet1 (1) (2))
(bitpacket Packet2 (A 8) (B))

; this work
(bitmatch `#(1 2 3)
  (((Packet1 bitpacket) (C 8)) (print "C=" C)))

; doesnt work
(bitmatch `#(1 2 3)
  (((Packet2 bitpacket) (C 8)) (print "A=" A)))


;(test-begin "string")
;(test 'ok (bitmatch "ABC" ((("A") (66) (#\C)) 'ok)))
;(test 'ok (bitmatch "ABC" ((("AB") (#\C)) 'ok)))
;(test-end)
;(test-begin "vector")
;(test 'ok (bitmatch `#( 65 66 67 ) (("A") (66) (#\C) -> 'ok)))
;(test-end)
;(test-begin "u8vector")
;(test 'ok (bitmatch `#u8( 65 66 67 ) (("A") (66) (#\C) -> 'ok)))
;(test-end)

(bitmatch `#( #x45 #x00 #x00 #x6c #x92 #xcc #x00 #x00
              #x38 #x06 #x00 #x00 #x92 #x95 #xba #x14 #xa9 #x7c #x15 #x95 )
  (((Version 4)
    (IHL 4)
    (TOS 8)
    (TL 16)
    (Identification 16)
    (Reserved 1) (DF 1) (MF 1)
    (FramgentOffset 13)
    (TTL 8)
    (Protocol 8) (check (or (= Protocol 1)
    	                    (= Protocol 2)
    	                    (= Protocol 6)
    	                    (= Protocol 17))) 
    (CheckSum 16)
    (SourceAddr 32 bitstring)
    (DestinationAddr 32 bitstring)
    (Optional bitstring))
    (begin
      (print "Version " Version)
      (print "IHL " IHL)
      (print "TL " TL)
      (print "Identification " Identification)
      (print "Reserver " Reserved " DF " DF " MF " MF)
      (print "FramgentOffset " FramgentOffset)
      (print "TTL " TTL)
      (print "Protocol " (case Protocol
		      	  ((1) "ICMP")
		      	  ((2) "IGMP")
		      	  ((6) "TCP")
		      	  ((17) "UDP")))
      (print "CheckSum " (sprintf "~X" CheckSum))
      (print "SourceAddr " (bitmatch SourceAddr
      	  		     (((a 8)(b 8)(c 8)(d 8))
      	  			(sprintf "~A.~A.~A.~A" a b c d))))
      (print "DestinationAddr " (bitmatch DestinationAddr
      	  	                   (((a)(b)(c)(d))
      	  	                     (sprintf "~A.~A.~A.~A" a b c d))))))
  
  (else
    (print "bad datagram")))

(bitmatch `#( 5 1 2 3 4 5)
  (((count 8) (rest (* count 8) bitstring))
    (print " count=" count " rest=" (bitstring-length rest))))
    
(test-begin "match")

(test 1.5
  (bitmatch `#( #x38 #x00 #x3f #x80 #x00 #x00 )
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
    ((#x8E) 'fail1)
    ((#x8C) 'fail2)
    ((#x8F) 'ok)))

(test 'ok
  (bitmatch `#( #x8F )
    ((#x8E) 'fail1)
    ((#x8C) 'fail2)
    (else 'ok)))

(test-end)

(test-begin "read")
(define bs (bitstring-of-vector `#(65 66 67)))
(test #f (bitstring-share bs 0 100))
(test 2 (bitstring->integer-big (bitstring-share bs 0 3)))
(test 5 (bitstring->integer-big (bitstring-share bs 3 10)))
(test 579 (bitstring->integer-big (bitstring-share bs 10 24)))
(test 2 (bitstring->integer-big (bitstring-read bs 3)))
(test 5 (bitstring->integer-big (bitstring-read bs 7)))
(test 579 (bitstring->integer-big (bitstring-read bs 14)))
(test #f (bitstring-read bs 1))
(define bs (bitstring-of-vector `#( #x8F )))
(test 1 (bitstring->integer-big (bitstring-share bs 0 1)))
(test 15 (bitstring->integer-big (bitstring-share bs 1 8)))
(define bs (bitstring-of-vector `#( #x7C #x00)))
(test 0 (bitstring->integer-big (bitstring-share bs 0 1)))
(test 31 (bitstring->integer-big (bitstring-share bs 1 6)))
(test-end)

(define (get-fields bs)
  (list (bitstring-offset bs) (bitstring-numbits bs) (bitstring-buffer bs)))

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
(test +inf.0 (bitstring->half (bitstring-of-vector `#( #x7C #x00))))
(test -inf.0 (bitstring->half (bitstring-of-vector `#( #xFC #x00))))
(test 0. (bitstring->half (bitstring-of-vector `#( #x00 #x00))))
(test -0. (bitstring->half (bitstring-of-vector `#( #x80 #x00))))
(test 0.5 (bitstring->half (bitstring-of-vector `#( #x38 #x00))))
(test 1. (bitstring->half (bitstring-of-vector `#( #x3C #x00))))
(test 25. (bitstring->half (bitstring-of-vector `#( #x4E #x40))))
(test 0.099976 (bitstring->half (bitstring-of-vector `#( #x2E #x66))))
(test -0.122986 (bitstring->half (bitstring-of-vector `#( #xAF #xDF))))
;-124.0625
(test-end)

(test-begin "single")
(test +inf.0 (bitstring->single (bitstring-of-vector `#( #x7F #x80 #x00 #x00))))
(test -inf.0 (bitstring->single (bitstring-of-vector `#( #xFF #x80 #x00 #x00))))
;(test +nan.0 (bitstring->single (bitstring-of-vector `#( #x7F #xC0 #x00 #x00))))
(test 0. (bitstring->single (bitstring-of-vector `#( #x00 #x00 #x00 #x00))))
(test -0. (bitstring->single (bitstring-of-vector `#( #x80 #x00 #x00 #x00))))
(test #t (equal? 1. (bitstring->single (bitstring-of-vector `#( #x3f #x80 #x00 #x00)))))
(test 0.5 (bitstring->single (bitstring-of-vector `#( #x3f #x00 #x00 #x00))))
(test 25. (bitstring->single (bitstring-of-vector `#( #x41 #xc8 #x00 #x00))))
(test 0.1 (bitstring->single (bitstring-of-vector `#( #x3d #xcc #xcc #xcd))))
(test -0.123 (bitstring->single (bitstring-of-vector `#( #xBD #xFB #xE7 #x6D))))
(test-end)

(test-exit)

