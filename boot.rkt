#lang racket/base
(require "main.rkt"
         "namespace.rkt"
         "binding.rkt"
         "primitives.rkt"
         "read-syntax.rkt"
         "module-path.rkt"
         racket/set
         racket/runtime-path)

(define-runtime-path startup.rktl "startup.rktl")

(define boot-ns (make-empty-core-namespace))
(namespace-require ''#%core boot-ns)

(define (copy-racket-module! name #:to [to-name name]
                             #:skip [skip-syms (seteq)]
                             #:alts [alts #hasheq()])
  (define mod-name `',name)
  (define-values (vars transes) (module->exports mod-name))
  (define syms (for/list ([sym (in-list (map car (cdr (assv 0 vars))))]
                          #:unless (set-member? skip-syms sym))
                 sym))
  (define to-resolved (make-resolved-module-path to-name))
  (define to-mpi (module-path-index-join (list 'quote to-name) #f))
  (declare-module!
   boot-ns
   to-resolved
   (make-module to-resolved
                #hasheqv()
                (hasheqv 0 (for/hash ([sym (in-list syms)])
                             (values sym
                                     (module-binding to-mpi 0 sym
                                                     to-mpi 0 sym
                                                     0))))
                0 0
                (lambda (ns phase-shift phase-level)
                  (when (= 0 phase-level)
                    (for ([sym (in-list syms)])
                      (or (hash-ref alts sym #f)
                          (namespace-set-variable! ns 0 sym (dynamic-require mod-name sym)))))))))

(copy-racket-module! '#%kernel
                     #:to '#%pre-kernel
                     #:skip primitive-ids
                     #:alts (hasheq 'eval eval
                                    'compile compile
                                    'expand expand
                                    'current-namespace current-namespace
                                    'make-empty-namespace make-empty-namespace
                                    'namespace-syntax-introduce namespace-syntax-introduce
                                    'namespace-require namespace-require))

(copy-racket-module! '#%paramz)
(copy-racket-module! '#%expobs)
(copy-racket-module! '#%foreign)
(copy-racket-module! '#%unsafe)
(copy-racket-module! '#%flfxnum)
(copy-racket-module! '#%network)
(copy-racket-module! '#%place)
(copy-racket-module! '#%futures)

(current-namespace boot-ns)

(define (eval-syntax s)
  (eval (compile (expand s))))

(define (eval-s-expr e)
  (eval-syntax (namespace-syntax-introduce (datum->syntax #f e))))

(call-with-input-file*
 startup.rktl
 (lambda (i)
   (port-count-lines! i)
   (for ([v (in-port (lambda (i) (read-syntax (object-name i) i)) i)])
     (eval-s-expr v))))

(use-compiled-file-paths null)
(current-eval (lambda (s)
                (if (syntax? s)
                    (eval-syntax s)
                    (eval-s-expr s))))
