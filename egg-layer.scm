(module egg-layer ()

(import scheme)
(import (chicken base)
        (chicken bitwise)
        (chicken file)
        (chicken format)
        (chicken io)
        (chicken irregex)
        (chicken load)
        (chicken pathname)
        (chicken port)
        (chicken pretty-print)
        (chicken process)
        (chicken process-context)
        (chicken sort)
        (chicken string))
(import egg-layer-params)

(define verbose? #f)

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
    (if (null? egg-entries)
        (die! "Egg not in egg index: ~a" egg)
        (car egg-entries))))

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

(define (shell-group . exprs)
  (sprintf "{ ~a ;}" (string-intersperse exprs " && ")))

(define (printer port fmt args)
  (apply fprintf (cons port (cons (string-append fmt "\n") args))))

(define (die! fmt . args)
  (printer (current-error-port) fmt args)
  (exit 1))

(define (info fmt . args)
  (when verbose?
    (printer (current-output-port) fmt args)))

(define (generate-makefile eggs force-dependencies? out-dir)
  (system* ((fetch-command) (egg-index-url) egg-index-compressed-filename))
  (system* ((uncompress-command) egg-index-compressed-filename
            egg-index-filename))
  (delete-file egg-index-compressed-filename)

  (define egg-index (read-egg-index egg-index-filename))

  (define visited '())

  (define (add-to-visited! egg)
    (set! visited (cons egg visited)))

  (define (visited? egg)
    (memq egg visited))

  (define (shell-unless-egg-installed egg shell-exp)
    (string-append
     (if verbose? "" "@")
     (if force-dependencies?
         shell-exp
         (shell-or (shell-egg-installed? egg) shell-exp))))

  (define (make-info egg message)
    (sprintf "echo '[~a] ~a'" egg message))

  (define (gen-egg-rules egg entry)
    ;; FIXME: this is a mess.  This procedure prints make rules and
    ;; returns a list of (target task phony?) elements.
    (if (visited? egg)
        '()
        (let* ((egg-version (get-egg-version entry))
               (egg-tarball-filename (sprintf "~a-~a.tar.gz" egg egg-version))
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
                      (shell-group
                       (make-info egg "Fetching")
                       ((fetch-command)
                        ((egg-tarball-url) egg egg-tarball-filename)
                        egg-tarball-filename))))

          (add-target! egg-checksum-file 'fetch-checksum #f)
          (make-rule egg-checksum-file '()
                     (shell-unless-egg-installed egg
                      (shell-group
                       (make-info egg "Fetching checksum file")
                       ((fetch-command)
                        ((egg-tarball-url)
                         egg
                         (string-append egg-tarball-filename ".sha1"))
                        egg-checksum-file))))

          (add-target! (make-target egg 'checksum) 'checksum #t)
          (make-rule (make-target egg 'checksum)
                     (list egg-tarball-filename egg-checksum-file)
                     (shell-unless-egg-installed egg
                      (shell-group
                       (make-info egg "Checking sum")
                       ((checksum-command) egg-checksum-file))))

          (add-target! egg-unpacked-dir 'unpack #f)
          (make-rule egg-unpacked-dir
                     (list (make-target egg 'checksum))
                     (shell-unless-egg-installed egg
                      (shell-group
                       (make-info egg "Unpacking")
                       ((extract-command) egg egg-tarball-filename))))

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
                      (shell-group
                       (make-info egg "Building and installing")
                       (sprintf "(cd ~a-~a && $(CHICKEN_INSTALL))"
                                egg egg-version))))
          targets)))

  (with-output-to-file (make-pathname out-dir "Makefile")
    (lambda ()
      (when verbose?
        (printf "CSC_OPTIONS ?= -verbose\n"))
      (printf "CSI ?= ~a\n" (csi-program))
      (printf "CHICKEN_INSTALL ?= ~a\n" (chicken-install-program))
      (printf "~a ?= ~a\n\n"
              "CHICKEN_REPOSITORY_PATH"
              "$(shell $(CSI) -p '(begin (import chicken.platform) (car (repository-path)))')")

      (printf "all: ~a\n\n"
              (string-intersperse
               (map (lambda (egg)
                      (make-target egg 'install))
                    eggs)))

      (let* ((entries (map (lambda (egg)
                             (get-egg-entry egg egg-index))
                           eggs))
             (deps (let loop ((entries entries))
                     (if (null? entries)
                         '()
                         (append (get-egg-deps (car entries))
                                 (loop (cdr entries))))))
             (targets '()))
        ;; Force the installation of the egg specified on the
        ;; command line (even if it is already installed)
        (set! targets
              (let loop ((eggs eggs)
                         (entries entries))
                (if (null? eggs)
                    '()
                    (append (gen-egg-rules (car eggs) (car entries))
                            (loop (cdr eggs) (cdr entries))))))
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
        ))))

(define (execute-action action dir)
  (unless (eqv? action 'none)
    (let ((parallelization
           (if (parallel-tasks)
               (sprintf " -j ~a" (parallel-tasks))
               " -j")))
      (change-directory dir)
      (let ((status (system (sprintf "make ~a~a" action parallelization))))
        (unless (zero? status)
          (exit (arithmetic-shift status -8)))))))

(define (usage exit-code)
  (let ((out (if (zero? exit-code)
                 (current-output-port)
                 (current-error-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf out "Usage: ~a [<options>] <egg> ...

<options>
  --action|-a <action>:
    Action to be executed (default: all).  Available actions:
    * all: install egg.
    * fetch: fetch egg and its dependencies.
    * none: only generate the Makefile.
    * unpack: fetch egg and its dependencies and unpack them.

  --config-file|-c <file>:
    Specify a configuration file alternative to the default one
    ($HOME/.egglayer.conf).

  --force-dependencies:
    Force processing actions for dependencies.  Without this option,
    actions for dependencies which are already installed will be skipped.

  --keep-output-directory|-k:
    By default, the output directory containing the Makefile generated by
    this program will be removed (unless --output-dir is provided or action
    is not `all').  With this option the the output directory is not removed.

  --output-dir|-o <dir>:
    Output directory (default: temporary directory with a random name)

  --parallel-tasks|-j <number>:
    The value of this parameter maps to the value of the -j parameter
    for make.  If given a negative value, -j for make will be given no
    value.

  --verbose:
    Print more information to the output.
" this)
    (exit exit-code)))


(let* ((args (command-line-arguments))
       (out-dir (create-temporary-directory))
       (config-file #f)
       (keep-output-dir? #f)
       (unset (list 'unset))
       (ntasks unset)
       (force-dependencies? #f)
       (valid-actions '(all fetch none unpack))
       (action 'all)
       (eggs '()))

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
                 (set! action user-action)
                 (unless (eqv? user-action 'all)
                   (set! keep-output-dir? #t)))
               (loop (cddr args)))
              ((member arg '("-c" "--config-file"))
               (when (null? (cdr args))
                 (die! "--config-file|-c requires an argument."))
               (set! config-file (cadr args))
               (loop (cddr args)))
              ((member arg '("-f" "--force-dependencies"))
               (set! force-dependencies? #t)
               (loop (cdr args)))
              ((member arg '("-j" "--parallel-tasks"))
               (when (null? (cdr args))
                 (die! "--parallel-tasks|-j requires an argument."))
               (let ((val (cadr args)))
                 (set! ntasks
                       (and-let* ((v (string->number val)))
                         (if (negative? v)
                             #f
                             val))))
               (loop (cddr args)))
              ((member arg '("-k" "--keep-output-dir"))
               (set! keep-output-dir? #t)
               (loop (cdr args)))
              ((member arg '("-o" "--output-dir"))
               (when (null? (cdr args))
                 (die! "--output-dir|-o requires an argument."))
               (set! keep-output-dir? #t)
               (set! out-dir (cadr args))
               (loop (cddr args)))
              ((member arg '("-v" "--verbose"))
               (set! verbose? #t)
               (loop (cdr args)))
              (else
               (let ((egg (string->symbol (car args))))
                 (unless (memq egg eggs)
                   (set! eggs (cons egg eggs))))
               (loop (cdr args)))))))

  (when (null? eggs)
    (usage 1))

  (info "Using ~a as output-directory." out-dir)

  (if config-file
      (begin
        (info "Loading configuration file: ~a" config-file)
        (load config-file))
      (let ((user-config (make-pathname (get-environment-variable "HOME")
                                        ".egg-layer.conf")))
        (when (file-exists? user-config)
          (info "Loading configuration file: ~a" user-config)
          (load user-config))))

  ;; Command line options clobber configuration from files
  (unless (eq? ntasks unset)
    (parallel-tasks ntasks))

  (create-directory out-dir 'recursively)
  (generate-makefile eggs force-dependencies? out-dir)
  (execute-action action out-dir)
  (if keep-output-dir?
      (printf "Sources of eggs and Makefile written into ~a\n" out-dir)
      (begin
        (info "Removing ~a." out-dir)
        (delete-directory out-dir 'recursively))))

) ;; end module
