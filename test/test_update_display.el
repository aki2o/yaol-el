(require 'test-simple)
(require 'cl-lib)
(require 'dash)
(require 'yaol)

(defun yaol-test-goto-next-annotation (name)
  (cl-loop with re = (format "@%s" name)
           while (not (eobp))
           if (and (eq (get-text-property (point) 'face) 'font-lock-comment-face)
                   (string-match re (buffer-substring (point-at-bol) (point-at-eol))))
           return t
           do (goto-char (or (next-single-property-change (point) 'face)
                             (point-max)))
           finally return nil))

(defun yaol-test-buffer-folded-string ()
  (apply 'concat
         (cl-loop initially (goto-char (point-min))
                  with ovs = (-sort (lambda (a b) (< (overlay-start a) (overlay-start b)))
                                    (yaol-overlays-in (point-min) (point-max)))
                  while (not (eobp))
                  for ov = (pop ovs)
                  append (remove nil (-flatten
                                      (list (buffer-substring-no-properties (point) (if ov (overlay-start ov) (point-max)))
                                            (substring-no-properties (or (and ov (overlay-get ov 'before-string)) ""))
                                            (substring-no-properties (or (and ov (overlay-get ov 'after-string)) "")))))
                  do (goto-char (if ov (overlay-end ov) (point-max))))))

(defun yaol-test-collect-cases (file)
  (with-current-buffer (find-file-noselect file)
    (cl-loop initially (goto-char (point-min))
             for mode = major-mode
             for test-start = (when (yaol-test-goto-next-annotation 'test)
                                (goto-char (next-single-property-change (point) 'face))
                                (point))
             for test-end = (when (and test-start
                                       (yaol-test-goto-next-annotation 'expect))
                              (previous-single-property-change (point) 'face))
             for expect-start = (when test-end
                                  (goto-char (next-single-property-change (point) 'face))
                                  (point))
             for expect-end = (when expect-start
                                (when (yaol-test-goto-next-annotation 'test)
                                  (goto-char (previous-single-property-change (point) 'face)))
                                (point))
             for test-code = (when (and test-start test-end)
                               (buffer-substring test-start test-end))
             for expect = (when (and expect-start expect-end)
                            (buffer-substring-no-properties expect-start expect-end))
             for test-buf = (get-buffer-create (format "*yaol:test_update_display:%s*" mode))
             for actual = (when test-code
                            (with-current-buffer test-buf
                              (insert test-code)
                              (funcall mode)
                              (yaol-fold-in-all-heads)
                              (yaol-test-buffer-folded-string)))
             for test = (when (and expect actual)
                          `(:expect ,expect :actual ,actual))
             while test
             collect test
             do (kill-buffer test-buf))))

(defun yaol-test-run ()
  (test-simple-start)
  (let ((yaol-fold-minimum-lines 1))
    (dolist (f (-remove (lambda (x)
                          (or (string-equal x (buffer-file-name))
                              (string-equal x (concat (buffer-file-name) "c"))))
                        (directory-files
                         "."
                         t
                         (rx-to-string `(and bos ,(file-name-sans-extension (file-name-nondirectory (buffer-file-name))) eow)))))
      (dolist (test (yaol-test-collect-cases f))
        (assert-equal (plist-get test :expect) (plist-get test :actual)))))
  (end-tests))

(yaol-test-run)
