
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

(bitmatch "\x05\x00ABCDE"
  (((Length 16 little)
    (StringData (* 8 Length) bitstring))
      (print "StringData:" (bitstring->list StringData)))
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

