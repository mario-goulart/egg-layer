(module egg-layer-params *

(import scheme)
(import (chicken base)
        (chicken format)
        (chicken pathname)
        (chicken process))

(define chicken-major-version 5)

;; External programs
(define fetch-command
  (make-parameter
   (lambda (url local-file)
     (sprintf "wget -qnv ~a -O ~a" (qs url) (qs local-file)))))

(define extract-command
  (make-parameter
   (lambda (tarball)
     (sprintf "tar -xzf ~a" (qs tarball)))))

(define uncompress-command
  (make-parameter
   (lambda (compressed-file #!optional uncompressed-file)
     (sprintf "gzip -dc ~a > ~a"
              (qs compressed-file)
              (qs (or uncompressed-file
                      (pathname-strip-extension compressed-file)))))))

(define checksum-command
  (make-parameter
   (lambda (file)
     (sprintf "sha1sum -c ~a" (qs file)))))

(define egg-index-url
  (make-parameter
   (sprintf "https://code.call-cc.org/egg-tarballs/~a/index.gz"
            chicken-major-version)))

(define egg-tarball-url
  (make-parameter
   (lambda (egg file)
     (sprintf "https://code.call-cc.org/egg-tarballs/~a/~a/~a"
              chicken-major-version
              egg
              file))))

)
