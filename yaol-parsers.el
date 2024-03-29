;;; yaol-parsers.el --- Parsers of yaol  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hiroaki Otsu

;; Author: Hiroaki Otsu <ootsuhiroaki@gmail.com>
;; Version: 0.0.1
;; Keywords: folding
;; URL: https://github.com/aki2o/yaol-el

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
(require 'yaol)
(require 'dash)
(require 'cl-lib)

;;;;;;;;;;;;
;; Parser

;;;###autoload
(defun yaol-indent-parser ()
  (yaol-new-nodes-by-indent))

;;;###autoload
(defun yaol-c-style-parser ()
  (let* ((open-positions (-> (yaol-collect-open-positions "{")
                              yaol-remove-non-program-position
                              yaol-expand-position-to-beginning-of-expression))
         (close-positions (-> (yaol-collect-close-positions "}")
                              yaol-remove-non-program-position)))
    (yaol-new-nodes-by-positions (append open-positions close-positions))))

(defun yaol-c-macro-parser ()
  (let ((open-positions (yaol-collect-open-positions "#if"))
        (close-positions (yaol-collect-close-positions "#endif")))
    (yaol-new-nodes-by-positions (append open-positions close-positions))))

;;;###autoload
(defun yaol-c-parser ()
  (append (yaol-c-style-parser) (yaol-c-macro-parser)))

;;; TODO: tag these nodes? have ability to manipulate nodes that are
;;; tagged? in a scoped fashion?
(defun yaol-javadoc-parser ()
  (let* ((doc-position? (lambda (pos) (yaol-has-any-face-position? pos '(font-lock-doc-face))))
         (open-positions (->> (yaol-collect-open-positions "/\\*\\*")
                              (-filter doc-position?)))
         (close-positions (->> (yaol-collect-close-positions "\\*/")
                               (-filter doc-position?))))
    (yaol-new-nodes-by-positions (append open-positions close-positions))))

;;;###autoload
(defun yaol-java-parser ()
  (append (yaol-c-style-parser) (yaol-javadoc-parser)))

(defun yaol-python-subparser (beg end)
  (goto-char beg)
  ;; iterate all same level children.
  (cl-loop while (and (beginning-of-defun -1) (<= (point) end)) ;; have children between beg and end?
	       for new-beg = (if (looking-back (rx (+ (any " \t"))) nil t)
                             (match-beginning 0)
                           (point))
		   for new-fold-beg = (progn (search-forward-regexp ":" nil t) (point))
		   for new-end = (progn (end-of-defun) (point))
	       collect (yaol-new-node new-beg new-end new-fold-beg new-end)
		   append (yaol-python-subparser new-beg new-end)
	       do (goto-char new-end)))

;;;###autoload
(defun yaol-python-parser ()
  (yaol-python-subparser (point-min) (point-max)))

(defun yaol-ruby-block-parser ()
  (let* ((open-re-maker (lambda (&rest words)
                          (rx-to-string `(and (or bol ";") (* (any " \t")) (group (or ,@words)) (or eol space "\\")))))
         (end-positions (-> (yaol-collect-close-positions (rx (or bol space) (group "end") (or eol space)))
                            yaol-remove-non-program-position))
         (do-positions (-> (yaol-collect-open-positions (rx (or bol space) (group "do") (or eol space)))
                           yaol-remove-non-program-position
                           yaol-expand-position-to-beginning-of-expression))
         (loop-positions (-> (yaol-collect-open-positions (funcall open-re-maker "for" "while" "until"))
                             yaol-remove-non-program-position
                             (yaol-expand-head-position :end-of-headers '(";" "do"))))
         (if-positions (-> (yaol-collect-open-positions (funcall open-re-maker "if" "unless"))
                           yaol-remove-non-program-position
                           (yaol-expand-head-position :end-of-headers '(";" "then"))))
         (other-positions (-> (yaol-collect-open-positions (funcall open-re-maker "class" "module" "def" "begin" "case"))
                              yaol-remove-non-program-position
                              yaol-expand-head-position)))
    (cl-loop for position in loop-positions
             ;; A loop syntax accepts expression with `do' like the following code.
             ;;
             ;; while (
             ;;   i += 1
             ;; ) < limit do
             ;;   something
             ;; end
             ;;
             ;; Therefore, reject it if next `do' has found before `end' or nested loop syntax start.
             for pt = (plist-get position :point)
             for do-points = (mapcar (lambda (x) (plist-get x :point)) do-positions)
             for end-points = (mapcar (lambda (x) (plist-get x :point)) end-positions)
             for loop-points = (mapcar (lambda (x) (plist-get x :point)) loop-positions)
             for do-point = (or (-first (lambda (x) (>= x pt)) do-points) (point-max))
             for end-point = (or (-first (lambda (x) (> x pt)) end-points) (point-max))
             for loop-point = (or (-first (lambda (x) (> x pt)) loop-points) (point-max))
             if (and (< do-point end-point)
                     (< do-point loop-point))
             do (setq do-positions
                      (-remove (lambda (x) (= (plist-get x :point) do-point)) do-positions)))
    (yaol-new-nodes-by-positions (append end-positions do-positions loop-positions if-positions other-positions))))

(defun yaol-ruby-paren-parser ()
  (let ((open-positions (-> (yaol-collect-open-positions (rx "["))
                            yaol-remove-non-program-position))
        (close-positions (-> (yaol-collect-close-positions (rx "]"))
                             yaol-remove-non-program-position)))
    (append
     (yaol-c-style-parser)
     (yaol-new-nodes-by-positions (append open-positions close-positions)))))

;;;###autoload
(defun yaol-ruby-parser ()
  (append (yaol-ruby-block-parser) (yaol-ruby-paren-parser)))

(defun yaol-lisp-parser (regex)
  (remove nil
          (cl-loop initially (progn (goto-char (point-min))
                                    (beginning-of-defun -1))
                   while (< (point) (point-max))
                   for beg = (point)
                   for fold-beg = (progn (search-forward-regexp regex nil t)
                                         (point))
                   for fold-end = (progn (end-of-defun)
                                         (backward-char) ;move point to one after the last paren
                                         (1- (point))) ;don't include the last paren in the fold
                   for end = (point)
                   collect (when (> fold-beg beg)
                             (yaol-new-node beg end fold-beg fold-end))
                   do (beginning-of-defun -1))))

;;;###autoload
(defun yaol-elisp-parser ()
  (yaol-lisp-parser "(def\\w*\\s-*\\(\\s_\\|\\w\\|[:?!]\\)*\\([ \\t]*(.*?)\\)?"))

;;;###autoload
(defun yaol-clj-parser ()
  (yaol-lisp-parser "(def\\(\\w\\|-\\)*\\s-*\\(\\s_\\|\\w\\|[?!]\\)*\\([ \\t]*\\[.*?\\]\\)?"))

;;;###autoload
(defun yaol-slim-parser ()
  (let* ((nodes (yaol-new-nodes-by-indent))
         (block-nodes (-filter (lambda (n)
                                 (string-match (rx bos (* space) (+ (any "a-z0-9_")) ":")
                                               (buffer-substring-no-properties (yaol-node-beg n) (yaol-node-fold-beg n))))
                               nodes)))
    (cl-loop for block-node in block-nodes
             do (setq nodes
                      (-remove (lambda (n)
                                 (and (not (yaol-node-equal? block-node n))
                                      (yaol-node-covered? block-node n)))
                               nodes)))
    nodes))

(defmacro yaol-define-markers-parser (name start-marker end-marker)
  "Create a parser for simple start and end markers."
  (let ((start-re (rx-to-string start-marker))
        (end-re (rx-to-string end-marker))
        (name (intern (format "yaol-%s-markers-parser" name))))
    `(defun ,name ()
       (let ((open-positions (yaol-collect-open-positions ,start-re))
             (close-positions (yaol-collect-close-positions ,end-re)))
         (yaol-new-nodes-by-positions (append open-positions close-positions))))))

;;;###autoload
(defun yaol-vim-like-markers-parser (_content))
(with-no-warnings
  (yaol-define-markers-parser "vim-like" "{{{" "}}}"))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major Head Regexp

;;;###autoload
(defvar yaol-go-major-head-regexp
  (rx bos (* space) (or "func" "type") space))

;;;###autoload
(defvar yaol-perl-major-head-regexp
  (rx bos (* space) (or "sub" "struct") space))

;;;###autoload
(defvar yaol-ruby-major-head-regexp
  (rx bos (* space)
      (or (or "class" "module" "def"
              "resources" "resource" "scope" "namespace")
          (and (? "RSpec.") (or "describe" "context" "xdescribe" "xcontext" "shared_examples_for" "shared_context"))
          (and (+ (any "a-zA-Z0-9:")) ".routes.draw"))
      space))

;;;###autoload
(defvar yaol-ruby-minor-head-regexp
  (rx bos (* space)
      (or "it" "before" "after")
      space))

;;;###autoload
(defvar yaol-ts-major-head-regexp
  (rx bos (* space)
      (or (and (or (and (? "export" (+ space)) (or "class" "interface" "type" "enum"))
                   (and (? (or "public" "private" "protected") (+ space)) (? "static" (+ space)) (or "async" "function" "async function")))
               space)
          (and (or "constructor" "describe" "context" "xdescribe" "xcontext")
               "("))))

;;;###autoload
(defvar yaol-ts-minor-head-regexp
  (rx bos (* space)
      (or "test" "it" "xtest" "xit" "beforeAll" "beforeEach" "afterAll" "afterEach")
      "("))

;;;###autoload
(defvar yaol-slim-major-head-regexp
  (rx bos (* space)
      (or (and (or "html" "head" "body" "table" "thead" "tbody" "ul" "ol" "dl" "form") eow) ; major html tag
          (and "-" (+ not-newline) " do ")                                                  ; maybo loop
          (and "-" (+ space) (or "if" "elsif" "unless" "for" "while" "until" "when") space) ; condition
          (and (or "=" "=>" "=<" "==" "==>" "==<") (+ space) (+ not-newline) " do "))))     ; maybe output with block


(provide 'yaol-parsers)
;;; yaol-parsers.el ends here
