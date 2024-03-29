;;; yaol.el --- Yet another outline  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Version: 0.0.1
;; Keywords: folding
;; URL: https://github.com/aki2o/yaol-el
;; Package-Requires: ((dash "2.5.0") (cl-lib "0.5") (log4e "0.4.1") (yaxception "1.0.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'dash)
(require 'cl-lib)
(require 'log4e)
(require 'yaxception)

(defgroup yaol nil
  "Yet another outline."
  :prefix "yaol-"
  :group 'convenience)

(defcustom yaol-parser-alist
  '((java-mode             . yaol-java-parser)
    (c-mode                . yaol-c-parser)
    (c++-mode              . yaol-c-style-parser)
    (perl-mode             . yaol-c-style-parser)
    (cperl-mode            . yaol-c-style-parser)
    (js-mode               . yaol-c-style-parser)
    (js2-mode              . yaol-c-style-parser)
    (js3-mode              . yaol-c-style-parser)
    (typescript-mode       . yaol-c-style-parser)
    (go-mode               . yaol-c-style-parser)
    (php-mode              . yaol-c-style-parser)
    (python-mode           . yaol-python-parser)
    (ruby-mode             . yaol-ruby-parser)
    (emacs-lisp-mode       . yaol-elisp-parser)
    (lisp-interaction-mode . yaol-elisp-parser)
    (clojure-mode          . yaol-clj-parser)
    (slim-mode             . yaol-slim-parser)
    (triple-braces         . yaol-vim-like-markers-parser))
  "Alist of parser in major-mode."
  :type '(list (cons symbol function))
  :group 'yaol)

(defcustom yaol-major-head-regexp-alist
  '((perl-mode       . yaol-perl-major-head-regexp)
    (cperl-mode      . yaol-perl-major-head-regexp)
    (ruby-mode       . yaol-ruby-major-head-regexp)
    (typescript-mode . yaol-ts-major-head-regexp)
    (go-mode         . yaol-go-major-head-regexp)
    (slim-mode       . yaol-slim-major-head-regexp))
  "Alist of regexp to filter major head in major-mode."
  :type '(list (cons symbol symbol))
  :group 'yaol)

(defcustom yaol-minor-head-regexp-alist
  '((ruby-mode       . yaol-ruby-minor-head-regexp)
    (typescript-mode . yaol-ts-minor-head-regexp))
  "Alist of regexp to filter minor head in major-mode."
  :type '(list (cons symbol symbol))
  :group 'yaol)

(defcustom yaol-major-level-alist
  '((typescript-mode       . 3)
    (ruby-mode             . 3)
    (emacs-lisp-mode       . 0)
    (lisp-interaction-mode . 0)
    (slim-mode             . 5))
  "Alist of number as major head level in major-mode."
  :type '(list (cons symbol (choice integer function)))
  :group 'yaol)

(defcustom yaol-fold-validate-function 'yaol-validate-node-fold-lines
  "Function to validate the node which should be folded."
  :type 'function
  :group 'yaol)

(defcustom yaol-fold-minimum-lines 5
  "Line size of folded region to fold by `yaol-validate-node-fold-lines'."
  :type 'integer
  :group 'yaol)

(defcustom yaol-fold-replacement "..."
  "Show this string instead of the folded text."
  :type 'string
  :group 'yaol)

(defface yaol-fold-replacement-face
  '((t :inherit font-lock-warning-face))
  "Face used to display the fold replacement text.")


(log4e:deflogger "yaol" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                  (error . "error")
                                                  (warn  . "warn")
                                                  (info  . "info")
                                                  (debug . "debug")
                                                  (trace . "trace")))
(yaol--log-set-level 'trace)

(defsubst yaol-do-until-resolved (mode fn)
  (cl-loop while mode
           for value = (funcall fn mode)
           for parent = (get mode 'derived-mode-parent)
           if value return value
           else do (setq mode parent)))

(defun yaol-head-regexp-for (category)
  (let* ((alist (cl-case category
                  (major yaol-major-head-regexp-alist)
                  (minor yaol-minor-head-regexp-alist)
                  (t (error "Unknown category : %s" category))))
         (value (yaol-do-until-resolved major-mode
                                        (lambda (m) (assoc-default m alist)))))
    (cond ((functionp value) (funcall value))
          ((symbolp value)   (eval value))
          (t                 value))))


;;;;;;;;;;
;; Node

(defsubst yaol-node-beg (node) (aref node 0))

(defsubst yaol-node-end (node) (aref node 1))

(defsubst yaol-node-fold-beg (node) (aref node 2))

(defsubst yaol-node-fold-end (node) (aref node 3))

(defsubst yaol-node-children (node) (aref node 4))

(defsubst yaol-node-parent (node) (aref node 5))

(defsubst yaol-node-is-root? (node) (not (yaol-node-parent node)))

(defsubst yaol-node-equal? (node1 node2)
  (and (= (yaol-node-beg node1) (yaol-node-beg node2)) (= (yaol-node-end node1) (yaol-node-end node2))))

(defsubst yaol-node-covered? (parent child)
  (and (<= (yaol-node-beg parent) (yaol-node-beg child)) (>= (yaol-node-end parent) (yaol-node-end child))))

(defsubst yaol-node-overlap? (node1 node2)
  (or (and (< (yaol-node-beg node1) (yaol-node-beg node2)) (> (yaol-node-end node1) (yaol-node-beg node2)))
      (and (< (yaol-node-beg node2) (yaol-node-beg node1)) (> (yaol-node-end node2) (yaol-node-beg node1)))
      (yaol-node-equal? node1 node2)
      (yaol-node-covered? node1 node2)
      (yaol-node-covered? node2 node1)))

(defsubst yaol-node-level (node)
  (let ((level 0))
    (while (not (yaol-node-is-root? node))
      (setq level (1+ level))
      (setq node (yaol-node-parent node)))
    level))

(defsubst yaol-node-on? (node point)
  (and (>= point (yaol-node-beg node)) (< point (yaol-node-end node))))

(defsubst yaol-node-head-on? (node point)
  (and (>= point (yaol-node-beg node)) (< point (yaol-node-fold-beg node))))

(defsubst yaol-node-body-on? (node point)
  (and (>= point (yaol-node-fold-beg node)) (< point (yaol-node-fold-end node))))

(defsubst yaol-node-tail-on? (node point)
  (and (>= point (yaol-node-fold-end node)) (< point (yaol-node-end node))))

(defsubst yaol-node-prefix-part (node)
  (buffer-substring (yaol-node-beg node) (yaol-node-fold-beg node)))

(defsubst yaol-node-body-part (node)
  (buffer-substring (yaol-node-fold-beg node) (yaol-node-fold-end node)))

(defsubst yaol-node-suffix-part (node)
  (buffer-substring (yaol-node-fold-end node) (yaol-node-end node)))


;;;;;;;;;;;;;;;;;;
;; Control Node

(defun yaol-set-node-children (parent children)
  (aset parent 4 children)
  (dolist (child children)
    (aset child 5 parent)))

(defun yaol-flat-nodes (nodes)
  (yaol--trace* "start flat nodes.")
  (remove nil (cl-loop for n in nodes
                       append (list n)
                       append (yaol-flat-nodes (yaol-node-children n)))))

(defun yaol-sort-nodes (nodes)
  (-sort (lambda (a b)
           (cond ((not (= (yaol-node-beg a) (yaol-node-beg b)))
                  (< (yaol-node-beg a) (yaol-node-beg b)))
                 ((not (= (yaol-node-fold-beg a) (yaol-node-fold-beg b)))
                  (< (yaol-node-fold-beg a) (yaol-node-fold-beg b)))
                 ((not (= (yaol-node-fold-end a) (yaol-node-fold-end b)))
                  (< (yaol-node-fold-end a) (yaol-node-fold-end b)))
                 (t
                  (< (yaol-node-end a) (yaol-node-end b)))))
         nodes))

(cl-defun yaol-sort-nodes-recursively (nodes &optional (flatten t))
  (yaol--trace* "start sort nodes recursively.")
  (when flatten
    (setq nodes (yaol-sort-nodes (yaol-flat-nodes nodes))))
  (cl-destructuring-bind (descendants parents)
      (-separate (lambda (node)
                   (-any? (lambda (n) (yaol-node-covered? n node))
                          (remove node nodes)))
                 nodes)
    (when descendants
      (cl-loop with children = (yaol-sort-nodes-recursively descendants nil)
               for parent in parents
               do (cl-destructuring-bind (covered-children other-children)
                      (-separate (lambda (child) (yaol-node-covered? parent child)) children)
                    (yaol-set-node-children parent covered-children)
                    (setq children other-children))))
    parents))


;;;;;;;;;;;;;;;;
;; Parser API

(cl-defun yaol-collect-positions (regex &key type start end)
  (save-excursion
    (cl-loop initially (goto-char (or start (point-min)))
             while (re-search-forward regex end t)
             for string = (or (match-string 1) (match-string 0))
             for point = (or (match-beginning 1) (match-beginning 0))
             collect `(:type ,type :string ,string :point ,point))))

(defun yaol-collect-open-positions (regexp)
  (yaol-collect-positions regexp :type 'open))

(defun yaol-collect-close-positions (regexp)
  (yaol-collect-positions regexp :type 'close))

(defun yaol-has-any-face-position? (position faces)
  (let ((face (get-text-property 0 'face (or (plist-get position :string) ""))))
    (-any? (lambda (f) (memq f faces))
           (if (listp face) face (list face)))))

(defun yaol-remove-non-program-position (positions)
  (cl-remove-if (lambda (pos)
                  (yaol-has-any-face-position? pos '(font-lock-doc-face
                                                     font-lock-comment-face
                                                     font-lock-string-face
                                                     font-lock-variable-name-face
                                                     font-lock-constant-face)))
                positions))

;; Helper to expand head range back to beginning of expression.
;;
;; ex)
;;   hash = {
;;     member1: value1
;;   }
;;
;; (-> (yaol-collect-open-positions "{")                ; => { ... }
;;     yaol-expand-position-to-beginning-of-expression) ; => hoge = { ... }
;;
(cl-defun yaol-expand-position-to-beginning-of-expression (positions &key (end-of-expressions '(";")))
  (save-excursion
    (-map (lambda (pos)
            (let* ((beg (progn
                          (goto-char (plist-get pos :point))
                          (-> (yaol-collect-positions (rx-to-string `(or ,@end-of-expressions)) :start (pos-bol) :end (point))
                              yaol-remove-non-program-position
                              -last-item)))
                   (beg (if beg
                            (+ (plist-get beg :point) (length (plist-get beg :string)))
                          (pos-bol))))
              `(:type ,(plist-get pos :type)
                      :string ,(concat (buffer-substring beg (plist-get pos :point)) (plist-get pos :string))
                      :point ,beg)))
          positions)))

;; Helper to expand head range to end of head.
;;
;; ex)
;;   class Something
;;     attr_accessor :member1
;;   end
;;
;; (-> (yaol-collect-open-positions "class") ; => class ... end
;;     yaol-expand-head-position)            ; => class Something ... end
;;
(cl-defun yaol-expand-head-position (positions &key (end-of-headers '()) (include-indent t))
  (save-excursion
    (-map (lambda (pos)
            (let* ((start (progn
                            (goto-char (plist-get pos :point))
                            (if (and include-indent
                                     (looking-back (rx (+ (any " \t"))) nil t))
                                (match-beginning 0)
                              (point))))
                   (end (when (> (length end-of-headers) 0)
                          (-> (yaol-collect-positions (rx-to-string `(or ,@end-of-headers)) :start start :end (pos-eol))
                              yaol-remove-non-program-position
                              -first-item)))
                   (end (if end
                            (+ (plist-get end :point) (length (plist-get end :string)))
                          (pos-eol))))
              `(:type ,(plist-get pos :type)
                      :string ,(buffer-substring start end)
                      :point ,start)))
          positions)))

(defun yaol-new-nodes-by-positions (positions)
  (cl-labels ((point-at-fold (pos)
                             (if (eq (plist-get pos :type) 'open)
                                 (+ (plist-get pos :point) (length (plist-get pos :string)))
                               (plist-get pos :point))))
    (remove nil
            (cl-loop with open-positions = nil
                     with positions = (-sort (lambda (a b) (< (point-at-fold a) (point-at-fold b)))
                                             positions)
                     while positions
                     for position = (pop positions)
                     for string = (plist-get position :string)
                     if (eq (plist-get position :type) 'open)
                     do (push position open-positions)
                     else if (and (eq (plist-get position :type) 'close)
                                  open-positions)
                     collect (let* ((open-position (pop open-positions))
                                    (beg (plist-get open-position :point))
                                    (fold-beg (+ beg (length (plist-get open-position :string))))
                                    (fold-end (plist-get position :point))
                                    (end (+ fold-end (length string))))
                               (yaol-new-node beg end fold-beg fold-end))))))

(defun yaol-new-nodes-by-indent ()
  (cl-labels ((get-current-level (string)
                                 (if (string-match (rx bos (group (+ space)) (not space)) string)
                                     (length (match-string 1 string))
                                   0)))
    (save-excursion
      (remove nil
              (cl-loop initially (goto-char (point-min))
                       with nodes          = nil
                       with open-positions = nil
                       with prev-point     = (pos-bol)
                       with prev-string    = (buffer-substring-no-properties prev-point (pos-eol))
                       with prev-level     = (get-current-level prev-string)
                       with curr-end       = (pos-eol)
                       until (eobp)
                       do (forward-line 1)
                       for point  = (pos-bol)
                       for string = (buffer-substring-no-properties point (pos-eol))
                       for level  = (get-current-level string)
                       if (or (string-match (rx (not blank)) string)
                              (= (pos-eol) (point-max)))
                       do (progn
                            (cond ((> level prev-level)
                                   (push `(:level ,prev-level :string ,prev-string :point ,prev-point) open-positions))
                                  ((< level prev-level)
                                   (cl-loop while (and open-positions
                                                       (<= level (plist-get (-first-item open-positions) :level)))
                                            for open-position = (pop open-positions)
                                            for beg           = (plist-get open-position :point)
                                            for fold-beg      = (+ beg (length (plist-get open-position :string)))
                                            do (push (yaol-new-node beg curr-end fold-beg curr-end) nodes))))
                            (setq prev-point  point)
                            (setq prev-string string)
                            (setq prev-level  level)
                            (setq curr-end    (pos-eol)))
                       finally return nodes)))))

(defun yaol-validate-node-fold-lines (node)
  (or (not yaol-fold-minimum-lines)
      (> (count-lines (yaol-node-fold-beg node) (yaol-node-fold-end node))
         yaol-fold-minimum-lines)))

(defun yaol-new-node (beg end fold-beg fold-end)
  (yaol--info* "start new node. beg[%d] end[%d] fold-beg[%d] fold-end[%d] : %s"
               beg end fold-beg fold-end (buffer-substring-no-properties beg fold-beg))
  (when (or (> beg fold-beg)
            (> fold-beg fold-end)
            (> fold-end end))
    (error "Invalid range of the node: beg=%d end=%d fold-beg=%d fold-end=%d" beg end fold-beg fold-end))
  ;; (when (ignore-errors (= (char-after end) ?\n))
  ;;   (yaol--trace* "include linefeed after end.")
  ;;   (setq end (1+ end)))
  (let* ((args (cl-loop for e in (list beg end fold-beg fold-end nil nil)
                        if e
                        do (setq e (let ((m (make-marker)))
                                     (set-marker m e)
                                     m))
                        collect e))
         (node (apply 'vector args))
         (regexps (list (yaol-head-regexp-for 'major)
                        (yaol-head-regexp-for 'minor))))
    (when (or (not (functionp yaol-fold-validate-function))
              (funcall yaol-fold-validate-function node)
              (-any? (lambda (regexp)
                       (when regexp (string-match regexp (buffer-substring-no-properties beg fold-beg))))
                     regexps))
      (yaol--trace* "created node.")
      node)))

(defun yaol-new-root-node (&optional children)
  (yaol--info* "start new root node. children[%d]" (length children))
  (let ((node (vector 1 most-positive-fixnum 1 most-positive-fixnum nil nil)))
    (yaol-set-node-children node (yaol-sort-nodes-recursively (remove nil children)))
    (yaol--trace* "created root node.")
    node))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Control Visibility

;; Create overlay with display-style which is nil or 'paragraph.
;;
;; ex)
;; function hoge {
;;   do_something
;;   do_otherwise
;; }
;;
;; - nil
;; function hoge {...}
;;
;; - paragraph
;; function hoge {
;;   ...
;; }
;;
(cl-defsubst yaol-create-overlay (node beg end &key display-style)
  (yaol--info* "start create overlay. beg[%d] end[%d] display-style[%s]" (marker-position beg) (marker-position end) display-style)
  (let* ((string (buffer-substring-no-properties beg end))
         (ov (when (and (< beg end)
                        (string-match (rx (not (any blank "\n"))) string))
               (make-overlay beg end))))
    (when ov
      (yaol--trace* "created overlay.")
      (overlay-put ov 'creator 'yaol)
      (overlay-put ov 'isearch-open-invisible 'yaol-isearch-open-invisible)
      (overlay-put ov 'isearch-open-invisible-temporary 'yaol-isearch-open-invisible-temporary)
      (overlay-put ov 'yaol-display-style display-style)
      (overlay-put ov 'yaol-node node)
      ov)))

(defun yaol-isearch-open-invisible (ov)
  (delete-overlay ov))

(defun yaol-isearch-open-invisible-temporary (ov hide-p)
  (overlay-put ov 'invisible (if hide-p 'yaol nil)))

(cl-defsubst yaol-set-overlay-in-current-level (node beg end &key head body child-head child-body)
  (let* ((covered-children (-filter (lambda (c)
                                      (and (<= beg (yaol-node-beg c)) (> end (yaol-node-beg c))))
                                    (yaol-node-children node)))
         (next-beg beg)
         results)
    (cl-loop with prev-child = nil
             while covered-children
             for child = (pop covered-children)
             for overlap? = (and prev-child (yaol-node-overlap? prev-child child))
             if (not overlap?)
             do (push (yaol-create-overlay node next-beg (yaol-node-beg child) :display-style 'paragraph) results)
             do (progn
                  (push (yaol-set-overlay-in-node child :head head :body body :child-head child-head :child-body child-body) results)
                  (when (or (not prev-child)
                            (> (yaol-node-end child) (yaol-node-end prev-child)))
                    (setq prev-child child))
                  (setq next-beg (yaol-node-end prev-child))))
    (push (yaol-create-overlay node next-beg end :display-style 'paragraph) results)
    (cond ((-all? (lambda (x) (not x)) results)
           ;; When show all range
           nil)
          ((-all? (lambda (x) (eq x t)) results)
           ;; When hide all range
           t)
          (t
           ;; When hide part of range
           'any))))

(defsubst yaol--visible-condition-p (value string)
  (cond ((numberp value) (> value 0))
        ((stringp value) (string-match value string))
        (t               value)))

(defsubst yaol--next-condition-value-from (value)
  (if (numberp value)
      (1- value)
    value))

(cl-defun yaol-set-overlay-in-node (node &key head body child-head child-body)
  (yaol--trace* "start set overlay in node. head[%s] body[%s] child-head[%s] child-body[%s] : %s"
                head body child-head child-body (yaol-node-prefix-part node))
  (let* ((show-children? (-any? (lambda (c)
                                  (or (yaol--visible-condition-p child-head (yaol-node-prefix-part c))
                                      (yaol--visible-condition-p child-body (yaol-node-body-part c))))
                                (yaol-node-children node)))
         (next-head child-head)
         (next-body child-body)
         (next-child-head (yaol--next-condition-value-from child-head))
         (next-child-body (yaol--next-condition-value-from child-body))
         (hide-part (lambda (beg end &optional display-style)
                      (if show-children?
                          (yaol-set-overlay-in-current-level node beg end :head next-head :body next-body :child-head next-child-head :child-body next-child-body)
                        (yaol-create-overlay node beg end :display-style display-style))))
         (show-body? (yaol--visible-condition-p body (yaol-node-body-part node)))
         (body-result (when (not show-body?)
                        (funcall hide-part (yaol-node-fold-beg node) (yaol-node-fold-end node))))
         (show-head? (cond ((not (eq body-result t)) t) ; should show if any body shown
                           (t                        (yaol--visible-condition-p head (yaol-node-prefix-part node)))))
         (prefix-result (when (not show-head?)
                          (funcall hide-part (yaol-node-beg node) (yaol-node-fold-beg node) 'paragraph)))
         (suffix-result (when (not show-head?)
                          (funcall hide-part (yaol-node-fold-end node) (yaol-node-end node)))))
    (cond ((-all? (lambda (x) (not x)) (list prefix-result body-result suffix-result))
           ;; When show all range
           nil)
          ((-all? (lambda (x) (eq x t)) (list prefix-result body-result suffix-result))
           ;; When hide all range
           t)
          (t
           ;; When hide part of range
           'any))))

(defun yaol-overlays-in (beg end)
  (-filter (lambda (ov)
             (eq (overlay-get ov 'creator) 'yaol))
           (overlays-in beg end)))

;; Remove overlays covered other overlay.
;;
;; ex)
;; function a {
;;   function b {
;;     do_something
;;   }
;; }
;;
;; when overlays set on the body of a and b, remove the overlay to cover b body.
(defun yaol-remove-duplicate-overlays (ovs)
  (yaol--trace* "start remove duplicate overlays.")
  (cl-loop for ov1 in ovs
           if (-any? (lambda (ov2)
                       (and (not (eq ov1 ov2))
                            (>= (overlay-start ov1) (overlay-start ov2))
                            (<= (overlay-end ov1) (overlay-end ov2))))
                     ovs)
           do (progn
                (setq ovs (remove ov1 ovs))
                (yaol--info* "removed duplicate overlay. beg[%d] end[%d]" (overlay-start ov1) (overlay-end ov1))
                (delete-overlay ov1))
           else
           collect ov1))

;; Merge multiple overlays to brige blank range.
;;
;; ex)
;; function a {
;;   do_something
;; }
;;
;; function b {
;;   do_otherwise
;; }
;;
;; when overlays set on the all of a and b, remove the overlay to cover b and expand the end of the overlay to cover a.
(defun yaol-merge-overlays (ovs)
  (yaol--trace* "start merge overlays.")
  (cl-loop with prev-ov = nil
           for ov in (-sort (lambda (a b)
                              (< (overlay-start a) (overlay-start b)))
                            ovs)
           for string = (when prev-ov
                          (buffer-substring (overlay-end prev-ov) (overlay-start ov)))
           if (and string
                   (string-match (rx bos (* (any blank "\n")) eos) string)
                   (<= (yaol-node-level (overlay-get prev-ov 'yaol-node)) (yaol-node-level (overlay-get ov 'yaol-node))))
           do (progn
                (yaol--info* "merged overlay. beg[%d] end[%d] -> beg[%d] end[%d]"
                             (overlay-start ov) (overlay-end ov) (overlay-start prev-ov) (overlay-end ov))
                (move-overlay prev-ov (overlay-start prev-ov) (overlay-end ov))
                (delete-overlay ov))
           else
           collect (prog1 ov
                     (setq prev-ov ov))))

(defun yaol-update-overlay-display (beg end)
  (yaol--info* "start update overlay display. beg[%d] end[%d]" beg end)
  (dolist (ov (-> (yaol-overlays-in beg end)
                  yaol-remove-duplicate-overlays
                  yaol-merge-overlays))
    (let* ((string (buffer-substring (overlay-start ov) (overlay-end ov)))
           (paragraph-style? (eq (overlay-get ov 'yaol-display-style) 'paragraph))
           (prefix (when (and paragraph-style?
                              (string-match (rx bos (+ (any "\n" blank))) string))
                     (let ((line (split-string (match-string 0 string) "\n")))
                       (concat (if (> (length line) 1) "\n" "")
                               (-last-item line)))))
           (suffix (when (and paragraph-style?
                              (string-match (rx "\n" (* blank) eos) string))
                     (match-string 0 string))))
      (yaol--trace* "set invisible. beg[%d] end[%d]\n[content]%s\n[prefix]%s\n[suffix]%s" (overlay-start ov) (overlay-end ov) string prefix suffix)
      (overlay-put ov 'invisible t)
      ;; NOTE:
      ;;   isearch-open-invisible seems to not work with display option.
      ;;   So, try to use before-string, after-string as substitute for display option.
      (overlay-put ov 'before-string (concat (or prefix "")
                                             (propertize yaol-fold-replacement 'face 'yaol-fold-replacement-face)))
      (overlay-put ov 'after-string suffix))))

(defun yaol-show-region (beg end)
  (remove-overlays beg end 'creator 'yaol))

(defun yaol-update-display (nodes &rest conditions-list)
  (let* ((beg (-min (-map 'yaol-node-beg nodes)))
         (end (-max (-map 'yaol-node-end nodes))))
    (yaol--info* "update display. beg[%d] end[%d]\n%s" beg end (mapconcat (lambda (c) (format "%s" c)) conditions-list "\n"))
    (yaol-show-region beg end)
    (dolist (c conditions-list)
      (dolist (node nodes)
        (yaol-set-overlay-in-node node
                                  :head (plist-get c :head)
                                  :body (plist-get c :body)
                                  :child-head (or (plist-get c :child-head) 0)
                                  :child-body (or (plist-get c :child-body) 0))))
    (yaol-update-overlay-display beg end)))

(defun yaol-folded-at? (point)
  (> (length (yaol-overlays-in point point)) 0))


;;;;;;;;;;;;;;;;;
;; Buffer Node

(defvar yaol-root-node nil)
(make-variable-buffer-local 'yaol-root-node)
(defvar yaol-modified-tick 0)
(make-variable-buffer-local 'yaol-modified-tick)
(defvar yaol-parser nil)
(make-variable-buffer-local 'yaol-parser)

(defun yaol-rebuild-required? (buffer)
  (or (not (buffer-local-value 'yaol-root-node buffer))
      (not (= (buffer-modified-tick buffer) (buffer-local-value 'yaol-modified-tick buffer)))))

(defun yaol-build-root-node (buffer)
  (yaol--info* "start build root node. buffer[%s]" buffer)
  (with-current-buffer buffer
    (let* ((mode (buffer-local-value 'major-mode buffer))
           (parser (or (yaol-do-until-resolved
                        mode
                        (lambda (m) (assoc-default m yaol-parser-alist)))
                       (buffer-local-value 'yaol-parser buffer)
                       (error "Not found parser for %s" (buffer-name buffer))))
           (nodes (or (if (yaol--log-debugging-p)
                          (yaxception:$
                            (yaxception:try
                              (save-excursion (funcall parser)))
                            (yaxception:catch 'error e
                              (yaol--fatal "failed to parse : %s\n%s" (yaxception:get-text e) (yaxception:get-stack-trace-string e))))
                        (ignore-errors (save-excursion (funcall parser))))
                      (error "Failed to parse yaol nodes from %s" (buffer-name)))))
      (setq yaol-root-node (yaol-new-root-node nodes))
      (setq yaol-modified-tick (buffer-modified-tick)))))

(cl-defun yaol-get-root-node (&optional (buffer (current-buffer)))
  (when (yaol-rebuild-required? buffer)
    (yaol-build-root-node buffer))
  (buffer-local-value 'yaol-root-node buffer))

(cl-defun yaol-find-deepest-nodes-at (point &optional include-hidden node-or-buffer)
  (let* ((node (cond ((not node-or-buffer)     (yaol-get-root-node))
                     ((bufferp node-or-buffer) (yaol-get-root-node node-or-buffer))
                     (t                        node-or-buffer)))
         (children (remove nil (cl-loop for child in (-filter (lambda (n) (yaol-node-on? n point))
                                                              (yaol-node-children node))
                                        append (yaol-find-deepest-nodes-at point include-hidden child))))
         (get-level (lambda (n)
                      (if (yaol-node-body-on? n point)
                          (1+ (yaol-node-level n))
                        (yaol-node-level n)))))
    (cond (children
           (let ((max-level (-max (-map get-level children))))
             (->> children
                  (-filter (lambda (n) (= (funcall get-level n) max-level)))
                  yaol-sort-nodes)))
          ((and (yaol-node-on? node point)
                (or include-hidden
                    (not (yaol-folded-at? (yaol-node-beg node)))))
           (list node)))))

(cl-defun yaol-find-nodes-from (node pred &key include-hidden (include-sibling t) (include-children t) (method '-find))
  (let* ((filter (lambda (n)
                   (and (funcall pred n)
                        (or include-hidden
                            (not (yaol-folded-at? (yaol-node-beg n)))))))
         (parent (yaol-node-parent node))
         (siblings (when (and parent include-sibling)
                     (funcall method filter (yaol-node-children parent))))
         (children (when include-children
                     (funcall method filter (yaol-node-children node))))
         (descendants (when include-children
                        (cl-loop for child in (yaol-node-children node)
                                 append (yaol-find-nodes-from child pred
                                                              :include-hidden include-hidden
                                                              :include-sibling nil
                                                              :method method)))))
    (remove nil (-flatten (list siblings children descendants)))))


;;;;;;;;;;;;;;;;;;
;; User Command

;;;###autoload
(defun yaol-fold-clear-all ()
  (interactive)
  (yaol-show-region (point-min) (point-max)))

;;;###autoload
(defun yaol-fold-clear-current ()
  (interactive)
  (let ((nodes (yaol-find-deepest-nodes-at (point))))
    (dolist (node nodes)
      (yaol-show-region (yaol-node-beg node) (yaol-node-end node)))))

;;;###autoload
(defun yaol-fold-root-heads ()
  (interactive)
  (let ((nodes (yaol-node-children (yaol-get-root-node))))
    (yaol-update-display nodes '(:head t))))

;;;###autoload
(cl-defun yaol-fold-major-heads (&key (level 0))
  (interactive)
  (let* ((regexp (or (yaol-head-regexp-for 'major) t))
         (major-level (or (yaol-do-until-resolved
                           major-mode
                           (lambda (m) (assoc-default m yaol-major-level-alist)))
                          0))
         (major-level (if (functionp major-level) (funcall major-level) major-level))
         (level (+ major-level level))
         (nodes (yaol-node-children (yaol-get-root-node))))
    (yaol-update-display nodes
                         `(:head t :child-head ,(1- level))
                         `(:head ,regexp :child-head ,regexp))))

;;;###autoload
(defun yaol-fold-current ()
  (interactive)
  (let ((nodes (yaol-find-deepest-nodes-at (point))))
    (yaol-update-display nodes '(:head t))))

;;;###autoload
(defun yaol-fold-all-descendant-heads ()
  (interactive)
  (let ((nodes (yaol-find-deepest-nodes-at (point))))
    (yaol-update-display nodes '(:head t :child-head t))))

;;;###autoload
(defun yaol-fold-major-descendant-heads ()
  (interactive)
  (let ((nodes (yaol-find-deepest-nodes-at (point)))
        (regexp (or (yaol-head-regexp-for 'major) t)))
    (yaol-update-display nodes `(:head t :child-head ,regexp))))

;;;###autoload
(defun yaol-fold-child-heads ()
  (interactive)
  (let ((nodes (yaol-find-deepest-nodes-at (point))))
    (yaol-update-display nodes '(:head t :child-head 1))))

;;;###autoload
(defun yaol-next-head ()
  (interactive)
  (let* ((pt (point))
         (next-nodes (yaol-find-nodes-from (yaol-get-root-node)
                                           (lambda (n) (> (yaol-node-beg n) pt))
                                           :include-sibling nil)))
    (if next-nodes
        (progn (goto-char (yaol-node-beg (-first-item (yaol-sort-nodes next-nodes))))
               (skip-syntax-forward " ")
               (point))
      (when (called-interactively-p 'any)
        (message "Not found next head."))
      nil)))

;;;###autoload
(defun yaol-previous-head ()
  (interactive)
  (let* ((pt (save-excursion
               (skip-syntax-backward " ")
               (point)))
         (next-nodes (yaol-find-nodes-from (yaol-get-root-node)
                                           (lambda (n) (< (yaol-node-beg n) pt))
                                           :include-sibling nil
                                           :method '-last)))
    (if next-nodes
        (progn (goto-char (yaol-node-beg (-last-item (yaol-sort-nodes next-nodes))))
               (skip-syntax-forward " ")
               (point))
      (when (called-interactively-p 'any)
        (message "Not found previous head."))
      nil)))

;;;###autoload
(defun yaol-next-sibling-head ()
  (interactive)
  (let* ((pt (point))
         (node (-last-item (yaol-find-deepest-nodes-at pt)))
         (next-node (-first-item (yaol-find-nodes-from node
                                                       (lambda (n) (> (yaol-node-beg n) pt))
                                                       :include-children nil))))
    (if next-node
        (progn (goto-char (yaol-node-beg next-node))
               (skip-syntax-forward " ")
               (point))
      (when (called-interactively-p 'any)
        (message "Not found next sibling head."))
      nil)))

;;;###autoload
(defun yaol-previous-sibling-head ()
  (interactive)
  (let* ((pt (save-excursion
               (skip-syntax-backward " ")
               (point)))
         (node (-last-item (yaol-find-deepest-nodes-at pt)))
         (next-node (-first-item (yaol-find-nodes-from node
                                                       (lambda (n) (< (yaol-node-beg n) pt))
                                                       :include-children nil
                                                       :method '-last))))
    (if next-node
        (progn (goto-char (yaol-node-beg next-node))
               (skip-syntax-forward " ")
               (point))
      (when (called-interactively-p 'any)
        (message "Not found previous sibling head."))
      nil)))

;;;###autoload
(defun yaol-up-head ()
  (interactive)
  (let ((parent (yaol-node-parent (-first-item (yaol-find-deepest-nodes-at (point))))))
    (if parent
        (progn (goto-char (yaol-node-beg parent))
               (skip-syntax-forward " ")
               (point))
      (when (called-interactively-p 'any)
        (message "Not found parent head."))
      nil)))

;;;###autoload
(defun yaol-down-head ()
  (interactive)
  (let* ((pt (point))
         (nodes (yaol-find-deepest-nodes-at pt))
         (next-nodes (remove nil
                             (cl-loop for node in nodes
                                      append (yaol-find-nodes-from node
                                                                   (lambda (n) (> (yaol-node-beg n) pt))
                                                                   :include-sibling nil)))))
    (if next-nodes
        (progn (goto-char (yaol-node-beg (-first-item (yaol-sort-nodes next-nodes))))
               (skip-syntax-forward " ")
               (point))
      (when (called-interactively-p 'any)
        (message "Not found down head."))
      nil)))


(provide 'yaol)
;;; yaol.el ends here
