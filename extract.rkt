#lang racket/base
(require racket/cmdline
         "set.rkt"
         "phase.rkt"
         "cache-for-boot.rkt"
         "compilation-unit.rkt"
         "serialize.rkt"
         "module-use.rkt"
         "binding.rkt"
         "runtime-primitives.rkt"
         (prefix-in new: "module-path.rkt"))

(define start-mod-path "main.rkt")

(define cache-dir
  (command-line
   #:once-any
   [("-l") module-path "Extract for <module-path>"
    (set! start-mod-path (string->symbol module-path))]
   #:args
   (cache-dir)
   cache-dir))

;; All relevant files must have been built and cached
;; via "boot.rkt"
(define cache (make-cache cache-dir))

;; We load each module's declation and phase-specific
;; compilation units once
(struct compiled-module (declaration phase-to-compilation-unit))
(define compiled-modules (make-hash))

;; Generic structure to represent a path+phase, where a "name"
;; corersponds to a `resolved-module-path-name` result; each
;; link corresponds to a "code"
(struct link (name phase) #:prefab)

;; A "code" is a phase-specific slide of a module; it corresponds to a
;; compilation unit, but we grup the compilation unit together with
;; phase-specific metadata from the declaration compilation unit
(struct code (compilation-unit ; the implementation
              imports          ; links for import "arguments"
              re-exports       ; links for variables re-exported
              variables        ; variables defined in the implementation; for detecting re-exports
              side-effects?))  ; whether the implementaiton has side effects other than variable definition

;; All compilation units that see we based on module
;; `requires`:
(define seen (make-hash)) ; link -> code

;; All compilation units that are non-empty:
(define codes (make-hash)) ; link -> code
;; The same codes are referenced this list, but kept in reverse
;; order of instantiation:
(define links-in-order null)

;; Which codes (as represented by a "link") are actually needed to run
;; the code, which includes anything referenced by the starting
;; point's exports and any imported code that has a side effect:
(define needed (make-hash)) ; link -> value for reason

;; Use the host Racket's module name resolver to normalize the
;; starting module path:
(define start-name
  (resolved-module-path-name
   (module-path-index-resolve
    (module-path-index-join start-mod-path #f))))

;; We always start at phase 0
(define start-link (link start-name 0))

;; Convert a module path index implemented by our compiler to
;; a module path index in the host Racket:
(define (build-native-module-path-index mpi wrt-name)
  (define-values (mod-path base) (new:module-path-index-split mpi))
  (cond
   [(not mod-path) (make-resolved-module-path wrt-name)]
   [else
    (module-path-index-join mod-path
                            (and base
                                 (build-native-module-path-index base wrt-name)))]))

;; Convert one of our module path indexes and a name to
;; the referenced name
(define (module-path-index->module-name mod name)
  (define p (build-native-module-path-index mod name))
  (resolved-module-path-name
   (if (resolved-module-path? p)
       p
       (module-path-index-resolve p))))

;; Get (possibly already-loaded) representation of a compiled module
;; from the cache
(define (get-compiled-module name root-name)
  (or (hash-ref compiled-modules name #f)
      (let ([local-name name])
                                        ;: Seeing this module for the first time
        (define cd (get-cached-compiled cache root-name void))
        (unless cd
          (error "unavailable in cache:" name))
        ;; For submodules, recur into the compilation directory:
        (define h (let loop ([cd cd] [name name])
                    (define h (compilation-directory->hash cd))
                    (if (or (not (pair? name))
                            (null? (cdr name)))
                        h
                        (loop (hash-ref h (encode-compilation-directory-key (cadr name)))
                              (cdr name)))))
        ;; Instantiate the declaration compilation unit
        (define decl (instantiate-compilation-unit (eval-compilation-unit (hash-ref h #""))
                                                   (list deserialize-instance)))
        ;; Make a `compiled-module` structure to represent the compilaed module
        ;; and all its compilation units (but not its submodules, although they're in `h`)
        (define comp-mod (compiled-module decl h))
        (hash-set! compiled-modules name comp-mod)
        comp-mod)))

(define (get-code! lnk)
  (define name (link-name lnk))
  (define phase (link-phase lnk))
  (define root-name (if (pair? name)
                        (car name)
                        name))
  (unless (or (symbol? root-name) ; skip pre-defined modules
              (hash-ref seen lnk #f))
    ;; Seeing this module+phase combination for the first time
    (log-error "Getting ~s at ~s" name phase)
    (define comp-mod (get-compiled-module name root-name))

    ;; Extract the relevant compilation unit (i.e., at a given phase)
    ;; from the compiled module
    (define compilation-unit
      (hash-ref (compiled-module-phase-to-compilation-unit comp-mod)
                (encode-compilation-directory-key phase)
                #f))

    ;; Extract other metadata at the module level:
    (define reqs (instance-variable-value (compiled-module-declaration comp-mod) 'requires))
    (define provs (instance-variable-value (compiled-module-declaration comp-mod) 'provides))

    ;; Extract phase-specific (i.e., compiliation-unit-specific) info on variables:
    (define vars (if compilation-unit
                     (list->set (compiled-compilation-unit-variables compilation-unit))
                     null))
    ;; Extract phase-specific (i.e., compiliation-unit-specific) info on side effects:
    (define side-effects? (and
                           (member phase (instance-variable-value (compiled-module-declaration comp-mod)
                                                                  'side-effects))
                           #t))
    ;; Extract phase-specific mapping of the compilation-unit arguments to modules
    (define uses
      (hash-ref (instance-variable-value (compiled-module-declaration comp-mod) 'phase-to-link-modules)
                phase
                null))
   
    (define dependencies
      (for*/list ([(req-phase reqs) (in-hash reqs)]
                  [req (in-list reqs)])
        (define at-phase 
          ;; we want whatever required module will have at this module's `phase`
          (phase- phase req-phase))
        (link (module-path-index->module-name req name)
              at-phase)))
    
    (define imports
      (for/list ([mu (in-list uses)])
        (link (let ([p (build-native-module-path-index (module-use-module mu) name)])
                (resolved-module-path-name
                 (if (module-path-index? p)
                     (module-path-index-resolve p)
                     p)))
              (module-use-phase mu))))

    (for ([dependency (in-list dependencies)])
      (get-code! dependency))
    
    (define re-exports
      (set->list
       (for*/set ([(sym binding) (in-hash (hash-ref provs phase #hasheq()))]
                  [l (in-value
                      (link (module-path-index->module-name (module-binding-module binding) name)
                            (module-binding-phase binding)))]
                  [re-c (in-value (hash-ref codes l #f))]
                  #:when (and re-c
                              (set-member? (code-variables re-c) (module-binding-sym binding))))
         l)))

    (define c (code compilation-unit imports re-exports vars side-effects?))

    (when (and (pair? imports)
               (not compilation-unit))
      (error "no implementation, but uses arguments?" name phase))
    
    (hash-set! seen lnk c)
    
    (when compilation-unit
      (hash-set! codes lnk c)
      (set! links-in-order (cons lnk links-in-order)))))

;; Compute which compilation units are actually used as imports
(define (needed! lnk reason)
  (unless (hash-ref needed lnk #f)
    (define c (hash-ref seen lnk #f))
    (when c
      (hash-set! needed lnk reason)
      (for ([in-lnk (in-list (code-imports c))])
        (needed! in-lnk lnk)))))

;; ----------------------------------------
;; Gather needed links

;; Start with the given link, and follow dependencies
(get-code! start-link)
(needed! start-link 'start)

;; We also want the starting name's re-exports:
(for ([ex-lnk (in-list (code-re-exports (hash-ref seen start-link)))])
  (needed! ex-lnk `(re-export ,start-link)))

;; Anything that shows up in `codes` with a side effect also counts
(for ([(l c) (in-hash codes)])
  (when (code-side-effects? c)
    (needed! l 'side-effect)))

;; ----------------------------------------
;; Report the results

(log-error "Traversed ~s modules" (hash-count compiled-modules))
(log-error "Got ~s relevant compilation units" (hash-count codes))
(log-error "Need ~s of those compilation units" (hash-count needed))

;; Check whether any nneded compilation unit needs a
;; an instance of a pre-defined instance that is not
;; part of the runtime system:
(for ([lnk (in-list links-in-order)])
  (define needed-reason (hash-ref needed lnk #f))
  (when needed-reason
    (define code (hash-ref codes lnk))
    (for ([in-lnk (in-list (code-imports code))])
      (define p (link-name in-lnk))
      (when (and (symbol? p)
                 (not (member p runtime-instances))
                 (hash-ref needed in-lnk #t))
        (log-error "~s needed by ~a at ~s\n  which is neeed by ~a"
                   p
                   (link-name lnk)
                   (link-phase lnk)
                   needed-reason)))))
