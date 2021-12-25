(module egg-layer ()

(import scheme)
(import (chicken base)
        (chicken file)
        (chicken format)
        (chicken io)
        (chicken irregex)
        (chicken pathname)
        (chicken port)
        (chicken pretty-print)
        (chicken process)
        (chicken process-context)
        (chicken sort)
        (chicken string))
(import egg-layer-params)

(define egg-index-compressed-filename "index.gz")

(define egg-index-filename "index")

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

(define (shell-egg-installed? egg)
  ;; egg is a symbol
  (sprintf "[ -e ~a ]"
           (qs (make-pathname "$(CHICKEN_REPOSITORY_PATH)"
                              (symbol->string egg) "egg-info"))))

(define (shell-or shell-exp1 shell-exp2)
  (sprintf "~a || ~a" shell-exp1 shell-exp2))

(define (die! fmt . args)
  (apply fprintf (cons (current-error-port)
                       (cons (string-append fmt "\n") args))))

(define (generate-makefile egg force-dependencies? out-dir)
  (let* ((egg-index (read-egg-index egg-index-filename))
         (entry (get-egg-entry egg egg-index))
         (visited '()))

    (define (shell-unless-egg-installed egg shell-exp)
      (if force-dependencies?
          shell-exp
          (shell-or (shell-egg-installed? egg) shell-exp)))

    (define (add-to-visited! egg)
      (set! visited (cons egg visited)))

    (define (visited? egg)
      (memq egg visited))

    (define (gen-egg-rules egg entry)
      ;; FIXME: this is a mess.  This procedure prints make rules and
      ;; returns a list of (target task phony?) elements.
      (if (visited? egg)
          '()
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
                       (shell-unless-egg-installed egg
                         ((fetch-command)
                          ((egg-tarball-url) egg egg-tarball-filename)
                          egg-tarball-filename)))

            (add-target! egg-checksum-file 'fetch-checksum #f)
            (make-rule egg-checksum-file '()
                       (shell-unless-egg-installed egg
                         ((fetch-command)
                          ((egg-tarball-url)
                           egg
                           (string-append egg-tarball-filename ".sha1"))
                          egg-checksum-file)))

            (add-target! (make-target egg 'checksum) 'checksum #t)
            (make-rule (make-target egg 'checksum)
                       (list egg-tarball-filename egg-checksum-file)
                       (shell-unless-egg-installed egg
                         ((checksum-command) egg-checksum-file)))

            (add-target! egg-unpacked-dir 'unpack #f)
            (make-rule egg-unpacked-dir
                       (list (make-target egg 'checksum))
                       (shell-unless-egg-installed egg
                         ((extract-command) egg-tarball-filename)))

            (add-target! (make-target egg 'install) 'install #t)
            (make-rule (make-target egg 'install)
                       (cons
                        egg-unpacked-dir
                        (let loop ((deps deps))
                          (if (null? deps)
                              '()
                              (let ((dep (car deps)))
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
                                      (loop (cdr deps)))))))
                       (shell-unless-egg-installed egg
                         (sprintf "(cd ~a-~a && chicken-install)"
                                  egg egg-version)))
            targets)))

    (with-output-to-file (make-pathname out-dir "Makefile")
      (lambda ()
        (printf "CHICKEN_REPOSITORY_PATH = ~a\n\n" (default-repository-path))

        (printf "all: ~a\n\n" (make-target egg 'install))

        (let* ((entry (get-egg-entry egg egg-index))
               (deps (get-egg-deps entry))
               (targets '()))
          ;; Force the installation of the egg specified on the
          ;; command line (even if it is already installed)
          (set! targets (gen-egg-rules egg entry))
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

(define (execute-action action dir)
  (unless (eqv? action 'none)
    (change-directory dir)
    (system* (sprintf "make ~a" action))))

(define (usage exit-code)
  (let ((out (if (zero? exit-code)
                 (current-output-port)
                 (current-error-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf out "Usage: ~a [<options>] <egg>

<options>
  --action|-a <action>:
    Action to be executed (default: all).  Available actions:
    * all: install egg.
    * fetch: fetch egg and its dependencies.
    * none: only generate the Makefile.
    * unpack: fetch egg and its dependencies and unpack them.

  --force-dependencies:
    Force processing actions for dependencies.  Without this option,
    actions for dependencies which are already installed will be skipped.

  --keep-output-directory|-k:
    By default, the output directory containing the Makefile generated by
    this program will be removed (unless --output-dir is provided).  With
    this option the the output directory is not removed.

  --output-dir|-o <dir>:
    Output directory (default: temporary directory with a random name)
" this)
    (exit exit-code)))


(let ((args (command-line-arguments))
      (out-dir (create-temporary-directory))
      (keep-output-dir? #f)
      (force-dependencies? #f)
      (valid-actions '(all fetch none unpack))
      (action 'all)
      (egg #f))

  ;; Command line parsing
  (let loop ((args args))
    (unless (null? args)
      (let ((arg (car args)))
        (cond ((member arg '("-h" "-help" "--help"))
               (usage 0))
              ((member arg '("-a" "--action"))
               (when (null? (cdr args))
                 (die! "--action|-a requires an argument."))
               (let ((user-action (string->symbol (cadr args))))
                 (unless (memq user-action valid-actions)
                   (die! "Invalid action: ~a" user-action))
                 (set! action user-action))
               (loop (cddr args)))
              ((member arg '("-f" "--force-dependencies"))
               (set! force-dependencies? #t)
               (loop (cdr args)))
              ((member arg '("-k" "--keep-output-dir"))
               (set! keep-output-dir? #t)
               (loop (cdr args)))
              ((member arg '("-o" "--output-dir"))
               (when (null? (cdr args))
                 (die! "--output-dir|-o requires an argument."))
               (set! keep-output-dir? #t)
               (set! out-dir (cadr args))
               (loop (cddr args)))
              (else
               (if (null? (cdr args))
                   (set! egg (string->symbol (car args)))
                   (usage 1)))))))

  (unless egg
    (usage 1))

  (printf "Using ~a as output-directory.\n" out-dir)

  (create-directory out-dir 'recursively)
  (system* ((fetch-command) (egg-index-url) egg-index-compressed-filename))
  (system* ((uncompress-command) egg-index-compressed-filename
                                 egg-index-filename))
  (delete-file egg-index-compressed-filename)
  (generate-makefile egg force-dependencies? out-dir)
  (execute-action action out-dir)
  (unless keep-output-dir?
    (printf "Removing ~a.\n" out-dir)
    (delete-directory out-dir 'recursively)))

) ;; end module
