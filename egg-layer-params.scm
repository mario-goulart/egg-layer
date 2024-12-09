(module egg-layer-params *

(import scheme)
(import (chicken base)
        (chicken irregex)
        (chicken foreign)
        (chicken format)
        (chicken pathname)
        (chicken platform)
        (chicken process))

(define chicken-major-version 5)

(define parallel-tasks
  ;; The value of this parameter maps to the value of the -j parameter
  ;; for make.  If #f, -j will be given no value.
  (make-parameter 1))

;; External programs
(define csi-program
  (make-parameter
   (make-pathname (list (foreign-value "C_INSTALL_PREFIX" c-string) "bin")
                  (foreign-value "C_CSI_PROGRAM" c-string))))

(define chicken-install-program
  (make-parameter
   (make-pathname (list (foreign-value "C_INSTALL_PREFIX" c-string) "bin")
                  (foreign-value "C_CHICKEN_INSTALL_PROGRAM" c-string))))

(define fetch-command
  (make-parameter
   (lambda (url local-file)
     (sprintf "wget -qnv ~a -O ~a" (qs url) (qs local-file)))))

(define extract-command
  (make-parameter
   (lambda (egg tarball)
     (sprintf "tar -xzf ~a" (qs tarball)))))

(define uncompress-command
  (make-parameter
   (lambda (compressed-file #!optional uncompressed-file)
     (sprintf "gzip -dc ~a > ~a"
              (qs compressed-file)
              (qs (or uncompressed-file
                      (pathname-strip-extension compressed-file)))))))

(define checksum-command
  (if (eq? (software-version) 'openbsd)
      (make-parameter
       (lambda (file)
         (sprintf "cat ~a | tr '\\t' ' ' | sha1 -c" (qs file))))
      (make-parameter
       (lambda (file)
         ;; Replace tab with double space for compatibility with busybox'
         ;; sha1sum.
         (sprintf "cat ~a | sed 's/\\t/  /' | sha1sum -c -" (qs file))))))

(define make-program
  (if (memq (software-version) '(dragonfly freebsd netbsd openbsd))
      (make-parameter "gmake")
      (make-parameter "make")))

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
