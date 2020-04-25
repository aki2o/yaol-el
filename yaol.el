;;; yaol.el --- Yet another outline  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Version: 0.0.1
;; Keywords: folding
;; URL: https://github.com/aki2o/yaol-el
;; Package-Requires: ((dash "2.5.0") (cl-lib "0.5") (log4e "0.3.3"))

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

(defcustom yaol-popular-head-regexp-alist
  '((perl-mode  . yaol-perl-popular-head-regexp)
    (cperl-mode . yaol-perl-popular-head-regexp)
    (go-mode    . yaol-go-popular-head-regexp)
    (ruby-mode  . yaol-ruby-popular-head-regexp)
    (slim-mode  . yaol-slim-popular-head-regexp))
  "Alist of regexp to filter popular head in major-mode."
  :type '(list (cons symbol symbol))
  :group 'yaol)

(defcustom yaol-popular-level-alist
  '((ruby-mode             . 1)
    (emacs-lisp-mode       . 0)
    (lisp-interaction-mode . 0)
    (slim-mode             . 5))
  "Alist of number as popular head level in major-mode."
  :type '(list (cons symbol (choice integer function)))
  :group 'yaol)

(defcustom yaol-default-popular-level 1
  "Number as papular head level in major-mode other than `yaol-popular-level-alist'."
  :type 'integer
  :group 'yaol)

(defcustom yaol-fold-validate-function 'yaol-validate-node-fold-lines
  "Function to validate the node which should be folded."
  :type 'function
  :group 'yaol)

(defcustom yaol-fold-minimum-lines 5
  "Line size to fold if folded region line size is over the value by `yaol-validate-node-fold-lines'."
  :type 'integer
  :group 'yaol)

(defcustom yaol-fold-replacement "..."
  ;; TODO: this should also be specifiable as a function: folded text -> string
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

(defun yaol-popular-head-regexp ()
  (let ((value (assoc-default major-mode yaol-popular-head-regexp-alist)))
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

(defun yaol-collect-positions (regex &optional type)
  (save-excursion
    (cl-loop initially (goto-char (point-min))
             while (re-search-forward regex nil t)
             for string = (or (match-string 1) (match-string 0))
             for point = (or (match-beginning 1) (match-beginning 0))
             collect `(:type ,type :string ,string :point ,point))))

(defun yaol-collect-open-positions (regexp)
  (yaol-collect-positions regexp 'open))

(defun yaol-collect-close-positions (regexp)
  (yaol-collect-positions regexp 'close))

(defun yaol-has-any-face-position? (position faces)
  (let ((face (get-text-property 0 'face (plist-get position :string))))
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
            (goto-char (plist-get pos :point))
            (if (re-search-backward (rx-to-string `(or bol ,@end-of-expressions)) nil t)
                `(:type ,(plist-get pos :type)
                        :string ,(concat (buffer-substring (match-end 0) (plist-get pos :point)) (plist-get pos :string))
                        :point ,(match-end 0))
              pos))
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
  (-map (lambda (pos)
          (let ((pt (progn
                      (goto-char (plist-get pos :point))
                      (if (and include-indent
                               (looking-back (rx (+ (any " \t"))) nil t))
                          (match-beginning 0)
                        (point)))))
            (re-search-forward (rx-to-string `(or eol ,@end-of-headers)) nil t)
            `(:type ,(plist-get pos :type)
                    :string ,(buffer-substring pt (point))
                    :point ,pt)))
        positions))

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
                       with prev-point     = (point-at-bol)
                       with prev-string    = (buffer-substring-no-properties prev-point (point-at-eol))
                       with prev-level     = (get-current-level prev-string)
                       with curr-end       = (point-at-eol)
                       until (eobp)
                       do (forward-line 1)
                       for point  = (point-at-bol)
                       for string = (buffer-substring-no-properties point (point-at-eol))
                       for level  = (get-current-level string)
                       if (or (string-match (rx (not blank)) string)
                              (= (point-at-eol) (point-max)))
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
                            (setq curr-end    (point-at-eol)))
                       finally return nodes)))))

(defun yaol-validate-node-fold-lines (node)
  (or (not yaol-fold-minimum-lines)
      (> (count-lines (yaol-node-fold-beg node) (yaol-node-fold-end node))
         yaol-fold-minimum-lines)))

(defun yaol-new-node (beg end fold-beg fold-end)
  (yaol--info* "start new node. beg[%s] end[%s] fold-beg[%s] fold-end[%s] : %s"
               beg end fold-beg fold-end (buffer-substring-no-properties beg fold-beg))
  (when (or (> beg fold-beg)
            (> fold-beg fold-end)
            (> fold-end end))
    (error "Invalid range of the node: beg=%s end=%s fold-beg=%s fold-end=%s" beg end fold-beg fold-end))
  (when (ignore-errors (= (char-after end) ?\n))
    (yaol--trace* "include linefeed after end.")
    (setq end (1+ end)))
  (let ((node (vector beg end fold-beg fold-end nil nil))
        (regexp (yaol-popular-head-regexp)))
    (when (or (not (functionp yaol-fold-validate-function))
              (funcall yaol-fold-validate-function node)
              (and regexp (string-match regexp (buffer-substring-no-properties beg fold-beg))))
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

(defun yaol-show-region (beg end)
  (remove-overlays beg end 'creator 'yaol))

(cl-defun yaol-handle-visibility (node &key head body (child-head 0) (child-body 0))
  (yaol--info* "start handle visibility. head[%s] body[%s] child-head[%s] child-body[%s] : %s"
               head body child-head child-body (buffer-substring-no-properties (yaol-node-beg node) (yaol-node-fold-beg node)))
  (cl-labels ((shown-child? (v s)
                            (cond ((numberp v) (> v 0))
                                  ((stringp v) (string-match v s))
                                  (t           v)))
              (next-value (v)
                          (if (numberp v) (> v 0) v))
              (next-child-value (v)
                                (if (numberp v) (1- v) v))
              (show-head? ()
                          (if (stringp head)
                              (string-match head (buffer-substring-no-properties (yaol-node-beg node) (yaol-node-fold-beg node)))
                            head))
              (show-body? ()
                          (if (stringp body)
                              (string-match body (buffer-substring-no-properties (yaol-node-fold-beg node) (yaol-node-fold-end node)))))
              (hide-part (show-children? beg end)
                         (if show-children?
                             (yaol-hide-current-level-parts beg end (yaol-node-children node))
                           (yaol-create-overlay beg end))))
    (let ((show-children? (-any? (lambda (c)
                                   (or (shown-child? child-head (buffer-substring-no-properties (yaol-node-beg c) (yaol-node-fold-beg c)))
                                       (shown-child? child-body (buffer-substring-no-properties (yaol-node-fold-beg c) (yaol-node-fold-end c)))))
                                 (yaol-node-children node))))
      (when (not (show-head?))
        (hide-part show-children? (yaol-node-beg node) (yaol-node-fold-beg node))
        (hide-part show-children? (yaol-node-fold-end node) (yaol-node-end node)))
      (when (not (show-body?))
        (hide-part show-children? (yaol-node-fold-beg node) (yaol-node-fold-end node)))
      (when show-children?
        (dolist (child (yaol-node-children node))
          (yaol-handle-visibility child
                                  :head (next-value child-head)
                                  :body (next-value child-body)
                                  :child-head (next-child-value child-head)
                                  :child-body (next-child-value child-body)))))))

(defun yaol-hide-current-level-parts (beg end children)
  (let* ((covered-children (-filter (lambda (c)
                                      (and (<= beg (yaol-node-beg c))
                                           (> end (yaol-node-beg c))))
                                    children))
         (next-beg beg))
    (cl-loop with prev-child = nil
             while covered-children
             for child = (pop covered-children)
             for overlap? = (and prev-child (yaol-node-overlap? prev-child child))
             if (not overlap?)
             do (yaol-create-overlay next-beg (yaol-node-beg child) :indent-keeping t)
             do (progn
                  (when (or (not prev-child)
                            (> (yaol-node-end child) (yaol-node-end prev-child)))
                    (setq prev-child child))
                  (setq next-beg (yaol-node-end prev-child))))
    (yaol-create-overlay next-beg end :indent-keeping t)))

(cl-defun yaol-create-overlay (beg end &key indent-keeping)
  (yaol--info* "start create overlay. beg[%s] end[%s] indent-keeping[%s]" beg end indent-keeping)
  (let* ((string (buffer-substring-no-properties beg end))
         (ov (when (and (< beg end)
                        (string-match (rx (not (any blank "\n"))) string))
               (make-overlay beg end)))
         (contiguous-folded? (when ov
                               (save-excursion
                                 (goto-char beg)
                                 (re-search-backward (rx (+ (any blank "\n")) point) nil t)
                                 (yaol-folded-at? (-max `(,(1- (point)) ,(point-min)))))))
         (before-string (when (and ov (not contiguous-folded?))
                          (propertize yaol-fold-replacement 'face 'yaol-fold-replacement-face)))
         (before-string (if (and before-string
                                 indent-keeping
                                 (string-match (rx bos "\n" (* blank)) string))
                            (concat (match-string 0 string) before-string)
                          before-string))
         (after-string (when (and ov
                                  indent-keeping
                                  (string-match (rx "\n" (* blank) eos) string))
                         (match-string 0 string))))
    (when ov
      (yaol--trace* "created overlay.\n[content]%s\n[before]%s\n[after]%s" string before-string after-string)
      (overlay-put ov 'creator 'yaol)
      (overlay-put ov 'invisible t)
      (overlay-put ov 'isearch-open-invisible 'yaol-isearch-open-invisible)
      (overlay-put ov 'isearch-open-invisible-temporary 'yaol-isearch-open-invisible-temporary)
      (when before-string
        (overlay-put ov 'before-string before-string))
      (when after-string
        (overlay-put ov 'after-string after-string))
      ov)))

(defun yaol-folded-at? (point)
  (-any? (lambda (ov)
           (eq (overlay-get ov 'creator) 'yaol))
         (overlays-at point)))

(defun yaol-isearch-open-invisible (ov)
  (overlay-put ov 'invisible nil))

(defun yaol-isearch-open-invisible-temporary (ov hide-p)
  (overlay-put ov 'invisible (if hide-p 'yaol nil)))


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
    (let* ((parser (or (assoc-default (buffer-local-value 'major-mode buffer) yaol-parser-alist)
                       (buffer-local-value 'yaol-parser buffer)
                       (error "Not found parser for %s" (buffer-name buffer))))
           (nodes (save-excursion
                    (funcall parser))))
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
(defun yaol-fold-in-all-heads ()
  (interactive)
  (yaol-show-region (point-min) (point-max))
  (dolist (node (yaol-node-children (yaol-get-root-node)))
    (yaol-handle-visibility node :head t :child-head t)))

;;;###autoload
(defun yaol-fold-in-popular-heads ()
  (interactive)
  (let ((regexp (or (yaol-popular-head-regexp) t)))
    (yaol-show-region (point-min) (point-max))
    (dolist (node (yaol-node-children (yaol-get-root-node)))
      (yaol-handle-visibility node :head regexp :child-head regexp))))

;;;###autoload
(defun yaol-fold-in-popular-level-heads ()
  (interactive)
  (let* ((regexp (or (yaol-popular-head-regexp) t))
         (level (assoc-default major-mode yaol-popular-level-alist))
         (level (cond ((functionp level) (funcall level))
                      (level             level)
                      (t                 yaol-default-popular-level))))
    (yaol-show-region (point-min) (point-max))
    (dolist (node (yaol-node-children (yaol-get-root-node)))
      (yaol-handle-visibility node :head regexp :child-head regexp)
      (yaol-handle-visibility node :head level :child-head level))))

;;;###autoload
(defun yaol-fold-current ()
  (interactive)
  (let ((nodes (yaol-find-deepest-nodes-at (point))))
    (dolist (node nodes)
      (yaol-show-region (yaol-node-beg node) (yaol-node-end node)))
    (dolist (node nodes)
      (yaol-handle-visibility node))))

;;;###autoload
(defun yaol-fold-in-all-descendant-heads ()
  (interactive)
  (let ((nodes (yaol-find-deepest-nodes-at (point))))
    (dolist (node nodes)
      (yaol-show-region (yaol-node-beg node) (yaol-node-end node)))
    (dolist (node nodes)
      (yaol-handle-visibility node :head t :child-head t))))

;;;###autoload
(defun yaol-fold-in-popular-descendant-heads ()
  (interactive)
  (let ((nodes (yaol-find-deepest-nodes-at (point)))
        (regexp (or (yaol-popular-head-regexp) t)))
    (dolist (node nodes)
      (yaol-show-region (yaol-node-beg node) (yaol-node-end node)))
    (dolist (node nodes)
      (yaol-handle-visibility node :head t :child-head regexp))))

;;;###autoload
(defun yaol-fold-in-child-heads ()
  (interactive)
  (let ((nodes (yaol-find-deepest-nodes-at (point))))
    (dolist (node nodes)
      (yaol-show-region (yaol-node-beg node) (yaol-node-end node)))
    (dolist (node nodes)
      (yaol-handle-visibility node :head t :child-head 1))))

;;;###autoload
(defun yaol-fold-in-child-heads-without-body ()
  (interactive)
  (let ((nodes (yaol-find-deepest-nodes-at (point))))
    (dolist (node nodes)
      (yaol-show-region (yaol-node-beg node) (yaol-node-end node)))
    (dolist (node nodes)
      (yaol-handle-visibility node :head t :body t :child-head 1))))

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
