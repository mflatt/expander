#lang racket/base
(require racket/private/config)

(provide utils-primitives)

;; These functions are a small step away from `#%kernel`, and they
;; have traditionally been available as the `#%utils` module. Don't
;; use `#%utils` in `racket/base`, since that's where the actual
;; implementation is. We turn the functions into a "primitive" module
;; using this table in a bootstrapped load.

(define utils-primitives
  (hasheq 'path-string? path-string?
          'normal-case-path normal-case-path
          'path-replace-extension path-replace-extension
          'path-add-extension path-add-extension
          'reroot-path reroot-path

          'path-list-string->path-list path-list-string->path-list
         
          'find-executable-path find-executable-path
         
          'call-with-default-reading-parameterization call-with-default-reading-parameterization
         
          'collection-path collection-path
          'collection-file-path collection-file-path
          'find-library-collection-paths find-library-collection-paths
          'find-library-collection-links find-library-collection-links
          
          'load/use-compiled load/use-compiled
          
          'find-main-config find-main-config
          'find-main-collects find-main-collects))
