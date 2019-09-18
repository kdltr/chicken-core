;; trivial test for catching runaway inlining (#1648), by
;; megane:

(module uri-generic
        (uri-relative-from)

        (import scheme)

        (define (uri-relative-from uabs base)
          (dif-segs-from uabs base))

        (define (dif-segs-from sabs base)
          (if (null? base)
              sabs
              (dif-segs-from sabs base))))
