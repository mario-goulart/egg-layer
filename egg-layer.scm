#!/bin/sh
#| -*- scheme -*-
exec csi -s $0 "$@"
|#

(import scheme)
(cond-expand
 (chicken-4
  (use data-structures irregex extras files ports posix utils)
  (define chicken-major-version 4)
  (define read-list read-file))
 (chicken-5
  (define chicken-major-version 5)
  (import (chicken file)
          (chicken format)
          (chicken io)
          (chicken irregex)
          (chicken pathname)
          (chicken platform)
          (chicken port)
          (chicken pretty-print)
          (chicken process)
          (chicken process-context)
          (chicken sort)
          (chicken string)))
 (else
  (error "Unsupported CHICKEN version.")))

;; External programs
(define fetcher "wget -nv ~a -O ~a")
(define tar "tar")
(define gzip "gzip")
(define sha1sum "sha1sum")

(define egg-index
  (sprintf "https://code.call-cc.org/egg-tarballs/~a/index.gz"
           chicken-major-version))

(define egg-index-compressed-filename "index.gz")

(define egg-index-filename "index")

(define egg-tarball-uri
  (sprintf "https://code.call-cc.org/egg-tarballs/~a/~~a"
           chicken-major-version))

(define (filter pred lst)
  (foldr (lambda (x r) (if (pred x) (cons x r) r)) '() lst))

(define (read-egg-index egg-index-file)
  (cdr (with-input-from-file egg-index-file read-list)))

(define get-egg-name car)
(define get-egg-version cadr)
(define get-egg-size caddr)
(define get-egg-checksum cadddr)
(define (get-egg-deps entry)
  (map (lambda (dep) ;; FIXME: handle versions
         (if (pair? dep)
             (car dep)
             dep))
       (list-ref entry 4)))
(define (get-egg-test-deps entry) (list-ref entry 5))

;; From setup-api (chicken-4.13.0)
(define (version>=? v1 v2)
  (define (version->list v)
    (map (lambda (x) (or (string->number x) x))
         (irregex-split "[-\\._]" (->string v))))
  (let loop ((p1 (version->list v1))
             (p2 (version->list v2)))
    (cond ((null? p1) (null? p2))
          ((null? p2))
          ((number? (car p1))
           (and (number? (car p2))
                (or (> (car p1) (car p2))
                    (and (= (car p1) (car p2))
                         (loop (cdr p1) (cdr p2))))))
          ((number? (car p2)))
          ((string>? (car p1) (car p2)))
          (else
           (and (string=? (car p1) (car p2))
                (loop (cdr p1) (cdr p2)))))))

(define (get-egg-entry egg egg-index #!optional version)
  ;; If version is not provided, get the latest
  ;; FIXME: actually use version
  (let ((egg-entries
         (sort
          (filter (lambda (entry)
                    (eqv? (get-egg-name entry) egg))
                  egg-index)
          (lambda (e1 e2)
            (version>=? (get-egg-version e1) (get-egg-version e2))))))
    (car egg-entries)))

(define (make-target egg task)
  (sprintf "task_~a_~a" egg task))

(define (make-rule target deps . actions)
  (printf "~a:~a\n~a\n\n"
          target
          (if (null? deps)
              ""
              (string-append " " (string-intersperse deps)))
          (string-intersperse
           (map (lambda (action)
                  (string-append "\t" action))
                actions)
           "\n")))

(define (egg-installed? egg)
  ;; egg is a symbol
  (cond-expand
   (chicken-4
    (let ((setup-info-files
           (glob (make-pathname (repository-path) "*.setup-info"))))
      (let loop ((setup-info-files setup-info-files))
        (if (null? setup-info-files)
            #f
            (and-let* ((setup-info-data
                        (with-input-from-file (car setup-info-files) read))
                       (egg-name (alist-ref 'egg-name setup-info-data))
                       (egg-name (string->symbol (car egg-name))))
              (or (eqv? egg-name egg)
                  (loop (cdr setup-info-files))))))))
   (chicken-5
    (file-exists?
     (make-pathname (repository-path) (symbol->string egg) "egg-info")))))

(define (filter-tasks targets task)
  ;; targets is a list of (target task phony?) elements
  (let loop ((targets targets))
    (if (null? targets)
        '()
        (let ((target (car targets)))
          (if (eq? (cadr target) task)
              (cons (car target)
                    (loop (cdr targets)))
              (loop (cdr targets)))))))

(define (ppe thing)
  (with-output-to-port (current-error-port)
    (cut pp thing)))

(define (die! fmt . args)
  (apply fprintf (cons (current-error-port)
                       (cons (string-append fmt "\n") args))))

(define (usage exit-code)
  (let ((out (if (zero? exit-code)
                 (current-output-port)
                 (current-error-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf out "Usage: ~a [<options>] <egg>

<options>
  -o <dir>:
     output directory (default: current directory)
" this)
    (exit exit-code)))


(let ((args (command-line-arguments))
      (out-dir (current-directory))
      (egg #f))

  ;; Command line parsing
  (let loop ((args args))
    (unless (null? args)
      (let ((arg (car args)))
        (cond ((member arg '("-h" "-help" "--help"))
               (usage 0))
              ((equal? arg "-o")
               (when (null? (cdr args))
                 (die! "-o requires an argument."))
               (set! out-dir (cadr args))
               (loop (cddr args)))
              (else
               (if (null? (cdr args))
                   (set! egg (string->symbol (car args)))
                   (usage 1)))))))

  (unless egg
    (usage 1))

  (create-directory out-dir 'recursively)
  (change-directory out-dir)
  (system* (sprintf fetcher (qs egg-index) egg-index-compressed-filename))
  (system* (sprintf "~a -dc ~a > ~a"
                    gzip
                    egg-index-compressed-filename
                    egg-index-filename))
  (delete-file egg-index-compressed-filename)

  (let* ((egg-index (read-egg-index egg-index-filename))
         (entry (get-egg-entry egg egg-index))
         (visited '()))

    (define (add-to-visited! egg)
      (set! visited (cons egg visited)))

    (define (visited? egg)
      (memq egg visited))

    (define (gen-egg-rules egg entry #!optional force-install?)
      ;; FIXME: this is a mess.  This procedure prints make rules and
      ;; returns a list of (target task phony?) elements.
      (if (and (not (visited? egg))
               (or force-install? (not (egg-installed? egg))))
          (let* ((egg-version (get-egg-version entry))
                 (egg-tarball-filename (sprintf "~a-~a.tar.gz"
                                                egg egg-version))
                 (egg-checksum-file (sprintf "~a.sha1" egg-tarball-filename))
                 (deps (get-egg-deps entry))
                 (egg-unpacked-dir (sprintf "~a-~a" egg egg-version))
                 (targets '()))

            (add-to-visited! egg)

            (define (add-target! target task phony?)
              (set! targets (cons (list target task phony?) targets)))

            (add-target! egg-tarball-filename 'fetch-tarball #f)
            (make-rule egg-tarball-filename '()
                       (sprintf fetcher
                                (sprintf egg-tarball-uri
                                         (conc egg "/" egg-tarball-filename))
                                egg-tarball-filename))

            (add-target! egg-checksum-file 'fetch-checksum #f)
            (make-rule egg-checksum-file '()
                       (sprintf fetcher
                                (sprintf egg-tarball-uri
                                         (conc egg "/" (string-append
                                                        egg-tarball-filename
                                                        ".sha1")))
                                egg-checksum-file))

            (add-target! (make-target egg 'checksum) 'checksum #t)
            (make-rule (make-target egg 'checksum)
                       (list egg-tarball-filename egg-checksum-file)
                       (sprintf "~a -c ~a" sha1sum egg-checksum-file))

            (add-target! egg-unpacked-dir 'unpack #f)
            (make-rule egg-unpacked-dir
                       (list (make-target egg 'checksum))
                       (sprintf "~a -xzf ~a" tar egg-tarball-filename))

            (add-target! (make-target egg 'install) 'install #t)
            (make-rule (make-target egg 'install)
                       (cons
                        egg-unpacked-dir
                        (let loop ((deps deps))
                          (if (null? deps)
                              '()
                              (let ((dep (car deps)))
                                (cond
                                 ((egg-installed? dep)
                                  (loop (cdr deps)))
                                 (else
                                  (unless (visited? dep)
                                    (set! targets
                                          (append
                                           targets
                                           (gen-egg-rules
                                            dep
                                            (get-egg-entry dep egg-index))))
                                    (add-to-visited! dep))
                                  (add-target! (make-target dep 'install) 'install #t)
                                  (cons (make-target dep 'install)
                                        (loop (cdr deps)))))))))
                       (sprintf "(cd ~a-~a && chicken-install)"
                                egg egg-version))
            targets)
          '()))

    (with-output-to-file "Makefile"
      (lambda ()
        (printf "all: ~a\n\n" (make-target egg 'install))

        (let* ((entry (get-egg-entry egg egg-index))
               (deps (get-egg-deps entry))
               (targets '()))
          ;; Force the installation of the egg specified on the
          ;; command line (even if it is already installed)
          (set! targets (gen-egg-rules egg entry 'force-install))
          (for-each (lambda (dep)
                      (get-egg-entry dep egg-index)
                      (set! targets
                            (append
                             targets
                             (gen-egg-rules dep (get-egg-entry dep egg-index)))))
                    deps)

          ;; Extra targets
          (printf "fetch: ~a\n\n"
                  (string-intersperse (filter-tasks targets 'checksum)))

          (printf "unpack: ~a\n\n"
                  (string-intersperse (filter-tasks targets 'unpack)))

          (print "clean:")
          (for-each
           (lambda (file)
             (print "\trm -rf " file))
           (append
            (filter-tasks targets 'fetch-tarball)
            (filter-tasks targets 'fetch-checksum)
            (filter-tasks targets 'unpack)))
          (newline)

          (print "help:")
          (print "\t@echo 'all: install the egg given as argument to egg-layer and its dependencies'")
          (print "\t@echo 'fetch: fetch all egg tarballs'")
          (print "\t@echo 'unpack: unpack all egg tarballs'")
          (print "\t@echo 'clean: remove all egg tarballs, checksum files and egg directories'")
          (newline)

          (printf ".PHONY: all clean help fetch unpack ~a\n"
                  (string-intersperse
                   (let loop ((targets targets))
                     (if (null? targets)
                         '()
                         (let ((target (car targets)))
                           (if (caddr target) ;; phony?
                               (cons (car target) (loop (cdr targets)))
                               (loop (cdr targets))))))))
          )))))
