
; Basic TGA image parser.
; Support True-Image type format and Run-Length-Encoding compression.
; SPEC: http://www.dca.fee.unicamp.br/~martino/disciplinas/ea978/tgaffs.pdf
;
; WARNING!!! bitpacket feature is experimental !!!

(use bitstring posix srfi-4)

(bitpacket TGA-Header
  (ID-length 8)
  (ColorMapType 8)
  (ImageType 8)
  (TGA-ColorMapSpec bitpacket)
  (TGA-ImageSpec bitpacket))

(bitpacket TGA-ColorMapSpec
  (FirstEntryIndex 16 little)
  (ColorMapLength 16 little)
  (ColorMapEntrySize 8))

(bitpacket TGA-ImageSpec
  (X-Origin 16 little)
  (Y-Origin 16 little)
  (ImageWidth 16 little)
  (ImageHeight 16 little)
  (PixelDepth 8)
  (ImageTransferOrder 2)
  (#x00 2) ; reserved
  (AttributesBitsPerPixel 4))

(define (bitstring->blob bs)
  (u8vector->blob (list->u8vector (bitstring->list bs))))

(define (parse-tga file file-out)
  (let* ((fi (file-open file (+ open/rdonly open/binary)))
         (fo (file-open file-out (+ open/write open/creat open/trunc open/binary)))
         (size (file-size fi))
         (res (file-read fi size))
         (data (car res)))
    (bitmatch data
      ; True-Color uncompressed
      (((TGA-Header bitpacket)
      	(check (and (= 0 ColorMapType) (= 2 ImageType)))
      	(ID-data ID-length bitstring)
        (Image-data (* ImageWidth ImageHeight PixelDepth) bitstring)
        (Rest-data bitstring))
        	(begin
        	  (print "True-Color uncompressed")
        	  (print ImageWidth "x" ImageHeight "x" PixelDepth)
        	  (parse-image-uncompressed
        	    (lambda (color)
        	      (file-write fo (bitstring->blob color)))
        	    PixelDepth Image-data)))
      ; True-Color compressed
      (((TGA-Header bitpacket)
      	(check (and (= 0 ColorMapType) (= 10 ImageType)))
      	(ID-data ID-length bitstring)
      	(Image-data bitstring))
      		(begin
      		  (print "True-Color compressed")
      		  (print ImageWidth "x" ImageHeight "x" PixelDepth)
      		  (parse-image-compressed
        	      (lambda (color)
        	      	(file-write fo (bitstring->blob color)))
        	      PixelDepth Image-data))))))

(define (parse-image-uncompressed func depth image)
  (bitmatch image
    ((())
      	'ok)
    (((Color depth bitstring) (Rest bitstring))
      (begin
      	(func Color)
      	(parse-image-uncompressed func depth Rest)))))

(define (parse-image-compressed func depth image)
  (bitmatch image
    ((())
      	'ok)
    (((1 1) (Count 7) (Color depth bitstring) (Rest bitstring))
      	(let loop ((i 0))
	  (func Color)
	  (if (< i Count)
	    (loop (+ i 1))
	    (parse-image-compressed func depth Rest))))
    (((0 1) (Count 7) (RAW-data (* depth (+ Count 1)) bitstring) (Rest bitstring))
      	(begin
      	  (parse-image-uncompressed func depth RAW-data)
      	  (parse-image-compressed func depth Rest)))))

; Convert images to raw pixels 
(parse-tga "tests/24compressed.tga" "tests/24c.raw")
(parse-tga "tests/24uncompressed.tga" "tests/24u.raw")

