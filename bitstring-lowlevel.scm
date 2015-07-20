(module bitstring-lowlevel
        (u8vector-not float->uint32 double->uint64 uint32->float uint64->double)
        (import scheme chicken foreign)

(define u8vector-not
   (foreign-primitive void ((u8vector data) (int size))
     "int i; for(i=0;i<size;++i) data[i] = ~data[i];"))

(define float->uint32
  (foreign-primitive void ((u8vector i) (float f))
    "*(uint32_t*)i = *(uint32_t*)&f;"))

(define double->uint64
  (foreign-primitive void ((u8vector i) (double d))
    "*(uint64_t*)i = *(uint64_t*)&d;"))

(define uint32->float
  (foreign-primitive float ((blob i))
    "C_return(*(float*)i);"))

(define uint64->double
  (foreign-primitive double ((blob i))
    "C_return(*(double*)i);"))

)
