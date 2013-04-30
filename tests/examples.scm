; Example 1. Tagged data structure.
;
; struct Tagged {
;  enum { IntegerType = 1, FloatType = 2 };
;  unsigned char Tag; // integer type = 1, float type = 2
;  union {
;   unsigned int IValue;
;   float FValue;
;  };
; };
;

(use bitstring)

; The following will print "integer:3721182122",
; which is the decimal value of #xDDCCBBAA
(bitmatch "\x01\xAA\xBB\xCC\xDD"
  (((#x01) (IValue 32 little))
      (print "integer:" IValue))
  (((#x02) (FValue 32 float))
      (print "float:" FValue)))

; Example 2. Fixed length string. 
;
; struct FixedString {
;  short Length; // length of StringData array
;  char StringData[0];
; };
;

(use bitstring)

; This will print "StringData:(65 66 67 68 69)"
; First it reads the length byte of 5, bind it to Length and
; then it will read a bit string with a length of that many octets.
(bitmatch "\x05\x00ABCDE"
  (((Length 16 little)
    (StringData (* 8 Length) bitstring))
      (print "StringData:" (bitstring->list StringData 8)))
  (else
      (print "invalid string")))

; Example 3. IP packet parsing. 
;

(use bitstring srfi-4)

(define IPRaw `#u8( #x45 #x00 #x00 #x6c
        #x92 #xcc #x00 #x00
        #x38 #x06 #x00 #x00
        #x92 #x95 #xba #x14
        #xa9 #x7c #x15 #x95 ))

(bitmatch IPRaw
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
      ; print packet filds
      (print "\n Version: " Version
             "\n IHL: " IHL
             "\n TOS: " TOS
             "\n TL:  " TL
             "\n Identification: " Identification
             "\n DF: " DF
             "\n MF: " MF
             "\n FramgentOffset: " FramgentOffset
             "\n Protocol: " Protocol
             "\n CheckSum: " CheckSum
             "\n SourceAddr: " 
                 (bitmatch SourceAddr (((A)(B)(C)(D)) (list A B C D)))
               "\n DestinationAddr: " 
                   (bitmatch DestinationAddr (((A)(B)(C)(D)) (list A B C D)))))
  (else
    (print "bad datagram")))

; Example 3.1 Using bitconstruct.

(define (construct-fixed-string str)
  (bitconstruct
    ((string-length str) 16) (str bitstring) ))

; The following will print "#t".  First, it reads a 16-bit number length
; and compares it to the immediate value of 7.  Then it will read a
; string and compare it to the immediate value of "qwerty.".  If there
; was any remaining data in the string, it would fail.
(bitmatch (construct-fixed-string "qwerty.")
  (((7 16) ("qwerty."))
    (print #t))
  (else 
    (print #f)))

; Example 3.2 Concatenating bitstrings.

(define (construct-complex-object)
  (bitconstruct
    ((construct-fixed-string "A") bitstring)
    (#xAABB 16)
    ((construct-fixed-string "RRR") bitstring)
    (#\X)))

(print (construct-complex-object))
