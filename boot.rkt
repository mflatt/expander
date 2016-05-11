#lang racket/base
(require "main.rkt"
         "namespace.rkt"
         "binding.rkt"
         "primitives.rkt"
         racket/set
         racket/runtime-path
         (prefix-in base: racket/base))

(define-runtime-path startup.rktl "startup.rktl")

(define boot-ns (make-empty-core-namespace))
(namespace-require ''#%core boot-ns)

(define (copy-racket-module! name #:to [to-name name] #:skip [skip-syms (seteq)])
  (define mod-name `',name)
  (define-values (vars transes) (module->exports mod-name))
  (define syms (for/list ([sym (in-list (map car (cdr (assv 0 vars))))]
                          #:unless (set-member? skip-syms sym))
                 sym))
  (declare-module!
   boot-ns
   to-name
   (make-module to-name
                #hasheqv()
                (hasheqv 0 (for/hash ([sym (in-list syms)])
                             (values sym
                                     (module-binding to-name 0 sym
                                                     to-name 0 sym
                                                     0))))
                0 0
                (lambda (ns phase-shift phase-level)
                  (when (= 0 phase-level)
                    (for ([sym (in-list syms)])
                      (namespace-set-variable! ns 0 sym (dynamic-require mod-name sym))))))))

(copy-racket-module! '#%kernel #:to '#%pre-kernel #:skip primitive-ids)
(copy-racket-module! '#%paramz)
(copy-racket-module! '#%expobs)
(copy-racket-module! '#%foreign)
(copy-racket-module! '#%unsafe)
(copy-racket-module! '#%flfxnum)
(copy-racket-module! '#%network)
(copy-racket-module! '#%place)
(copy-racket-module! '#%futures)

(define (eval-module e)
  (parameterize ([current-namespace boot-ns])
    (define c (compile (expand (namespace-syntax-introduce (datum->syntax #f e)))))
    (eval c)))

(define (syntax->syntax v)
  (cond
   [(base:syntax? v)
    (datum->syntax #f
                   (syntax->syntax (base:syntax-e v))
                   (vector (base:syntax-source v)
                           (base:syntax-line v)
                           (base:syntax-column v)
                           (base:syntax-position v)
                           (base:syntax-span v)))]
   [(pair? v) (cons (syntax->syntax (car v))
                    (syntax->syntax (cdr v)))]
   [else v]))

(call-with-input-file*
 startup.rktl
 (lambda (i)
   (port-count-lines! i)
   (for ([v (in-port (lambda (i) (read-syntax (object-name i) i)) i)])
     (eval-module (syntax->syntax v)))))
