;;; poly-helm-mode-tests.el --- Tests for poly-helm-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'poly-helm-mode)

(defvar poly-helm-mode-test-template-path
  (expand-file-name "examples/test-template.yaml" (file-name-directory load-file-name))
  "Path to the test template file.")

(defun poly-helm-mode-test--get-face-at-pos (pos)
  "Return the face at position POS."
  (get-text-property pos 'face))

(defun poly-helm-mode-test-all-patterns ()
  "Test poly-helm-mode detection with various file patterns."
  (interactive)
  (let ((test-cases '(
                      ;; Should use poly-helm-mode
                      "/path/to/project/values.yaml"
                      "/path/to/project/values.yml"
                      "/path/to/project/Chart.yaml"
                      "/path/to/project/Chart.yml"
                      "/path/to/project/templates/deployment.yaml"
                      "/path/to/project/templates/service.yml"
                      "/path/to/project/charts/subchart/templates/configmap.yaml"
                      "/path/to/project/helm-charts/templates/ingress.yml"

                      ;; Should use yaml-mode
                      "/path/to/project/config.yaml"
                      "/path/to/project/docker-compose.yml"
                      "/path/to/project/src/config.yaml"
                      "/path/to/project/data.yml"
                      "/path/to/project/other-values.yaml"
                      "/path/to/project/my-chart.yaml")))
    (message "Testing poly-helm-mode file detection:")
    (dolist (test-file test-cases)
      (let ((original-buffer-file-name buffer-file-name))
        (setq buffer-file-name test-file)
        (let ((result (poly-helm-mode-is-helm-file-p)))
          (setq buffer-file-name original-buffer-file-name)
          (message "  %s -> %s"
                   (file-name-nondirectory test-file)
                   (if result "poly-helm-mode" "yaml-mode")))))))

(defun poly-helm-mode-test-detection (&optional filepath)
  "Test poly-helm-mode file detection logic.
If FILEPATH is provided, test that specific path.
Otherwise, test the current buffer's file."
  (interactive)
  (let* ((test-file (or filepath buffer-file-name))
         (original-buffer-file-name buffer-file-name))
    (if test-file
        (progn
          ;; Temporarily set buffer-file-name for testing
          (setq buffer-file-name test-file)
          (let ((result (poly-helm-mode-is-helm-file-p)))
            (setq buffer-file-name original-buffer-file-name)
            (message "File: %s -> %s"
                     test-file
                     (if result "poly-helm-mode" "yaml-mode"))
            result))
      (message "No file to test (buffer has no associated file)")
      nil)))

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
