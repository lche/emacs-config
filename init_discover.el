;; init_discover.el
;; Lauren Che
;; Apr 11 2016

(discover-add-context-menu
 :context-menu '(pan-find-decoder
              (description "Find PAN decoder")
              (lisp-switches
               ("-cf" "Case should fold search" case-fold-search t nil))
              (lisp-arguments
               ("=l" "context lines to show (occur)"
                "list-matching-lines-default-context-lines"
                (lambda (dummy) (interactive) (read-number "Number of context lines to show: "))))
              (actions
               ("pan-find-decoder"
                ("s" "Look in src/" pan-find-decoder-src)
                ("i" "Look in include/" pan-find-decoder-include))))
 :bind "C-x p")

