(load "bitstring.scm")

(use bitstring test)

(test-begin "string")
(test 'ok (bitmatch "ABC" (("A") (66) (#\C) -> 'ok)))
(test 'ok (bitmatch "ABC" (("AB") (#\C) -> 'ok)))
(test-end)

(test-begin "vector")
(test 'ok (bitmatch `#( 65 66 67 ) (("A") (66) (#\C) -> 'ok)))
(test-end)

(test-begin "u8vector")
(test 'ok (bitmatch `#u8( 65 66 67 ) (("A") (66) (#\C) -> 'ok)))
(test-end)

(test 1.5
  (bitmatch `#( #x38 #x00 #x3f #x80 #x00 #x00 )
    ((let a 16 float) (let b 32 float) ->
      (begin (print "a=" a " b=" b) (+ a b)))))

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

(bitmatch `#( 5 1 2 3 4 5)
  ((let count 8) (let rest (* count 8) bitstring) ->
    (print " count=" count " rest=" (bitstring-length rest))))
    
(bitmatch `#( #x45 #x00 #x00 #x6c #x92 #xcc #x00 #x00
              #x38 #x06 #x00 #x00 #x92 #x95 #xba #x14 #xa9 #x7c #x15 #x95 )
  ((let Version 4)
   (let IHL 4)
   (let TOS 8)
   (let TL 16)
   (let Identification 16)
   (let Reserved 1) (let DF 1) (let MF 1)
   (let FramgentOffset 13)
   (let TTL 8)
   (let Protocol 8) (check (or (= Protocol 1)
       				 (= Protocol 2)
       				 (= Protocol 6)
       				 (= Protocol 17))) 
   (let CheckSum 16)
   (let SourceAddr 32 bitstring)
   (let DestinationAddr 32 bitstring)
   (let Optional bitstring) ->
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
      	  		     ((let a 8)(let b 8)(let c 8)(let d 8) ->
      	  			(sprintf "~A.~A.~A.~A" a b c d))))
      (print "DestinationAddr " (bitmatch DestinationAddr
      	  	                   ((let a)(let b)(let c)(let d) ->
      	  			   (sprintf "~A.~A.~A.~A" a b c d))))))
  
  (else
    (print "bad datagram")))

(test-begin "match")
(test (list 1 15)
  (bitmatch `#( #x8F )
    ((let flagBit 1 big) (let restValue 7) -> (list flagBit restValue))))

(test 'ok
  (bitmatch `#( #x8F )
    ((1 1) (let rest) -> 'fail)
    ((let x 1) (check (= x 0)) (let rest bitstring) -> 'fail2) 
    ((1 1) (let rest bitstring) -> 'ok)))

(test 'ok
  (bitmatch `#( #x8F )
    ((#x8E) -> 'fail1)
    ((#x8C) -> 'fail2)
    ((#x8F) -> 'ok)))

(test 'ok
  (bitmatch `#( #x8F )
    ((#x8E) -> 'fail1)
    ((#x8C) -> 'fail2)
    (else 'ok)))

(test-error
  (bitmatch `#( #x8F )
    ((#x8E) -> 'fail1)
    ((#x8C) -> 'fail2)
    (else 'ok)
    ((#x8F) -> 'fail3)))
    	
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

