;;; yaol-config.el --- Config of yaol  -*- lexical-binding: t -*-

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
(require 'sequential-command)
(require 'owdriver)
(require 'hydra)

(defcustom yaol-config-hydra-idle nil
  "Idle time for `defhydra'."
  :type 'number
  :group 'yaol)

(with-no-warnings
  (define-sequential-command yaol-config-cycle-heads
                             yaol-fold-major-heads
                             yaol-config-fold-major-heads-1
                             yaol-config-fold-major-heads-2
                             yaol-fold-root-heads)

  (define-sequential-command yaol-config-cycle-current-heads
                             yaol-fold-all-descendant-heads
                             yaol-fold-major-descendant-heads
                             yaol-fold-child-heads)

  (define-sequential-command yaol-config-toggle-current
                             yaol-fold-clear-current
                             yaol-fold-current)
  )

(owdriver-define-command yaol-config-cycle-heads)
(owdriver-define-command yaol-config-cycle-current-heads)
(owdriver-define-command yaol-config-toggle-current)
(owdriver-define-command yaol-fold-clear-all)
(owdriver-define-command yaol-up-head)
(owdriver-define-command yaol-next-sibling-head)
(owdriver-define-command yaol-previous-sibling-head)
(owdriver-define-command yaol-down-head)
(owdriver-define-command scroll-up-command)
(owdriver-define-command scroll-down-command)

(add-to-list 'owdriver-keep-driving-commands 'yaol-config-cycle-heads t)
(add-to-list 'owdriver-keep-driving-commands 'yaol-config-cycle-current-heads t)
(add-to-list 'owdriver-keep-driving-commands 'yaol-config-toggle-current t)
(add-to-list 'owdriver-keep-driving-command-prefixes "yaol-" t)

;;;###autoload
(defun yaol-config-fold-major-heads-1 ()
  (interactive)
  (yaol-fold-major-heads :level -1))

;;;###autoload
(defun yaol-config-fold-major-heads-2 ()
  (interactive)
  (yaol-fold-major-heads :level -2))

;;;###autoload
(defun yaol-config-hydra/body () (interactive))
(fmakunbound 'yaol-config-hydra/body)
(with-no-warnings
  (defhydra yaol-config-hydra (:hint nil :idle yaol-config-hydra-idle
                                     :post (yaol-fold-clear-all))
   "
^Toggle^                      ^Move^
^^^^^^--------------------------------------------
_s_: cycle headings           _h_: go to parent
_f_: cycle current headings   _j_: go to next
_c_: toggle current           _k_: go to prev
_a_: show all                 _l_: go to child
^ ^                           _J_: scroll up
^ ^                           _K_: scroll down
"
   ("s" yaol-config-cycle-heads)
   ("f" yaol-config-cycle-current-heads)
   ("c" yaol-config-toggle-current)
   ("a" yaol-fold-clear-all)
   ("h" yaol-up-head)
   ("j" yaol-next-sibling-head)
   ("k" yaol-previous-sibling-head)
   ("l" yaol-down-head)
   ("J" scroll-up-command)
   ("K" scroll-down-command)
   ("q" nil "quit")))

;;;###autoload
(defun yaol-config-hydra-other-window/body () (interactive))
(fmakunbound 'yaol-config-hydra-other-window/body)
(with-no-warnings
  (defhydra yaol-config-hydra-other-window (:hint nil :idle yaol-config-hydra-idle
                                                  :post (owdriver-do-yaol-fold-clear-all))
   "
^Toggle^                      ^Move^
^^^^^^--------------------------------------------
_s_: cycle headings           _h_: go to parent
_f_: cycle current headings   _j_: go to next
_c_: toggle current           _k_: go to prev
_a_: show all                 _l_: go to child
^ ^                           _J_: scroll up
^ ^                           _K_: scroll down
"
   ("s" owdriver-do-yaol-config-cycle-heads)
   ("f" owdriver-do-yaol-config-cycle-current-heads)
   ("c" owdriver-do-yaol-config-toggle-current)
   ("a" owdriver-do-yaol-fold-clear-all)
   ("h" owdriver-do-yaol-up-head)
   ("j" owdriver-do-yaol-next-sibling-head)
   ("k" owdriver-do-yaol-previous-sibling-head)
   ("l" owdriver-do-yaol-down-head)
   ("J" owdriver-do-scroll-up-command)
   ("K" owdriver-do-scroll-down-command)
   ("q" nil "quit")))

(provide 'yaol-config)
;;; yaol-config.el ends here
