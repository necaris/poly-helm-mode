;;; poly-helm-mode-tests.el --- Tests for poly-helm-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'poly-helm-mode)

(defvar poly-helm-mode-test-template-path
  (expand-file-name "examples/test-template.yaml" (file-name-directory load-file-name))
  "Path to the test template file.")

(defun poly-helm-mode-test--get-face-at-pos (pos)
  "Return the face at position POS."
  (get-text-property pos 'face))

(ert-deftest poly-helm-mode-highlighting-test ()
  "Test basic syntax highlighting in poly-helm-mode."
  (with-temp-buffer
    (insert-file-contents poly-helm-mode-test-template-path)
    (poly-helm-mode)
    (font-lock-ensure)

    ;; Test '{{' delimiter at line 4
    (goto-line 4)
    (search-forward "{{")
    (should (equal (poly-helm-mode-test--get-face-at-pos (match-beginning 0)) 'poly-helm-template-delimiter-face))

    ;; Test 'if' keyword at line 11
    (goto-line 11)
    (search-forward "if")
    (should (equal (poly-helm-mode-test--get-face-at-pos (match-beginning 0)) 'poly-helm-template-keyword-face))

    ;; Test '.Values' variable at line 11
    (goto-line 11)
    (search-forward ".Values")
    (should (equal (poly-helm-mode-test--get-face-at-pos (match-beginning 0)) 'poly-helm-template-variable-face))
    
    ;; Test 'end' keyword at line 13
    (goto-line 13)
    (search-forward "end")
    (should (equal (poly-helm-mode-test--get-face-at-pos (match-beginning 0)) 'poly-helm-template-keyword-face))
    ))

(provide 'poly-helm-mode-tests)

;;; poly-helm-mode-tests.el ends here
