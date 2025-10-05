;;; poly-helm-mode-tests.el --- Tests for poly-helm-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'poly-helm-mode)

(ert-deftest test-pattern-detection ()
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

(ert-font-lock-deftest-file test-highlighting
  "Test that font-locking works correctly."
  ;; TODO: Figure out why innermode chunks are only fontified when you enter
  ;; the first one; something is being done lazily but I don't know what!!
  poly-helm-mode "examples/test-template.yaml")
   
(provide 'poly-helm-mode-tests)

;;; poly-helm-mode-tests.el ends here
