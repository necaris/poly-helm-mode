;;; helm-mode-tests.el --- Tests for helm-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'helm-mode)

(defvar helm-mode-test-template-path
  (expand-file-name "test-template.yaml" (file-name-directory load-file-name))
  "Path to the test template file.")

(defun helm-mode-test--get-face-at-pos (pos)
  "Return the face at position POS."
  (get-text-property pos 'face))

(ert-deftest helm-mode-highlighting-test ()
  "Test basic syntax highlighting in helm-mode."
  (with-temp-buffer
    (insert-file-contents helm-mode-test-template-path)
    (helm-mode)
    (font-lock-ensure)

    ;; Test '{{' delimiter at line 4
    (goto-line 4)
    (search-forward "{{")
    (should (equal (helm-mode-test--get-face-at-pos (match-beginning 0)) 'helm-template-delimiter-face))

    ;; Test 'if' keyword at line 11
    (goto-line 11)
    (search-forward "if")
    (should (equal (helm-mode-test--get-face-at-pos (match-beginning 0)) 'helm-template-keyword-face))

    ;; Test '.Values' variable at line 11
    (goto-line 11)
    (search-forward ".Values")
    (should (equal (helm-mode-test--get-face-at-pos (match-beginning 0)) 'helm-template-variable-face))
    
    ;; Test 'end' keyword at line 13
    (goto-line 13)
    (search-forward "end")
    (should (equal (helm-mode-test--get-face-at-pos (match-beginning 0)) 'helm-template-keyword-face))
    ))

(provide 'helm-mode-tests)

;;; helm-mode-tests.el ends here
