;;; poly-helm-mode-tests.el --- Tests for poly-helm-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'poly-helm-mode)

(defvar poly-helm-mode-test-template-path
  (expand-file-name "examples/test-template.yaml" (file-name-directory load-file-name))
  "Path to the test template file.")

(defun string-at-point ()
  "Get the string (without properties) just before point."
  (buffer-substring-no-properties (- (point) 1) (point)))

(defun face-at-point ()
  "Get the font-face of the character _just before_ point."
  (get-text-property (- (point) 1) 'face))

(ert-deftest poly-helm-mode-pattern-detection-test ()
  "Test poly-helm-mode detection with various file patterns."
  (let ((test-cases '(;; Should use poly-helm-mode
                      ("/path/to/project/values.yaml" . t)
                      ("/path/to/project/values.yml" . t)
                      ("/path/to/project/Chart.yaml" . t)
                      ("/path/to/project/Chart.yml" . t)
                      ("/path/to/project/templates/deployment.yaml" . t)
                      ("/path/to/project/templates/service.yml" . t)
                      ("/path/to/project/charts/subchart/templates/configmap.yaml" . t)
                      ("/path/to/project/helm-charts/templates/ingress.yml" . t)
                      ;; Should use yaml-mode
                      ("/path/to/project/config.yaml" . nil)
                      ("/path/to/project/docker-compose.yml" . nil)
                      ("/path/to/project/src/config.yaml" . nil)
                      ("/path/to/project/data.yml" . nil)
                      ("/path/to/project/other-values.yaml" . nil)
                      ("/path/to/project/my-chart.yaml" . nil))))

    (dolist (test-spec test-cases)
      (let ((original-buffer-file-name buffer-file-name))
        (cl-destructuring-bind (test-file . is-helm) test-spec
          (setq buffer-file-name test-file)
          (let ((result (poly-helm-mode-is-helm-file-p)))
            (setq buffer-file-name original-buffer-file-name)
            (should (equal result is-helm))))))))

(ert-deftest poly-helm-mode-highlighting-test ()
  "Test basic syntax highlighting in poly-helm-mode."
  (with-temp-buffer
    (insert-file-contents poly-helm-mode-test-template-path)
    (poly-helm-mode)
    (font-lock-ensure)

    ;; Test '{{' delimiter at line 4
    (goto-line 4)
    (search-forward "{{")
    (should (equal (string-at-point) "{"))
    (should (equal (text-properties-at (point)) nil))

    (should (equal (face-at-point) 'poly-helm-template-delimiter-face))

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
    (should (equal (poly-helm-mode-test--get-face-at-pos (match-beginning 0)) 'poly-helm-template-keyword-face))))

(provide 'poly-helm-mode-tests)

;;; poly-helm-mode-tests.el ends here
