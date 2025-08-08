;;; poly-helm-mode.el --- Major mode for Helm templates -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Rami Chowdhury <rami.chowdhury@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (polymode "0.2.2") (yaml-mode "0.0.15"))
;; Keywords: languages, helm, kubernetes, yaml, templates
;; URL: https://github.com/Rami-Chowdhury/poly-helm-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides syntax highlighting for Helm templates, which combine
;; YAML syntax with Go template directives ({{ }} blocks).
;;
;; Helm templates are used in Kubernetes package management and contain:
;; - Standard YAML structure and syntax
;; - Go template actions in {{ }} delimiters
;; - Template functions and pipelines
;; - Control structures like {{ if }}, {{ range }}, etc.
;;
;; Usage:
;;   (require 'poly-helm-mode)
;;   ;; Files will automatically use poly-helm-mode if they are:
;;   ;; - Named values.yaml/yml or Chart.yaml/yml
;;   ;; - Located in templates/ or charts/ directories
;;
;; Testing:
;;   M-x eval-buffer
;;   M-x poly-helm-mode-test-all-patterns

;;; Code:

(require 'polymode)
(require 'yaml-mode)

(defgroup poly-helm-mode nil
  "Major mode for editing Helm templates."
  :group 'languages
  :prefix "poly-helm-mode-")

(defcustom poly-helm-mode-template-directories '("templates" "charts")
  "List of directory names that indicate Helm template files."
  :type '(repeat string)
  :group 'poly-helm-mode)

(defface poly-helm-template-delimiter-face
  '((t :inherit font-lock-preprocessor-face :weight bold))
  "Face for Helm template delimiters {{ and }}."
  :group 'poly-helm-mode)

(defface poly-helm-template-action-face
  '((t :inherit font-lock-function-name-face))
  "Face for Helm template actions and functions."
  :group 'poly-helm-mode)

(defface poly-helm-template-variable-face
  '((t :inherit font-lock-variable-name-face))
  "Face for Helm template variables."
  :group 'poly-helm-mode)

(defface poly-helm-template-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for Helm template keywords like if, range, with."
  :group 'poly-helm-mode)

(defvar poly-helm-template-keywords
  '("if" "else" "end" "range" "with" "template" "define" "block"
    "include" "required" "default" "empty" "fail" "printf" "print"
    "println" "quote" "squote" "nindent" "indent" "trim" "upper"
    "lower" "title" "untitle" "repeat" "substr" "trunc" "abbrev"
    "abbrevboth" "initials" "randAlphaNum" "randAlpha" "randNumeric"
    "randAscii" "wrap" "wrapWith" "contains" "hasPrefix" "hasSuffix"
    "split" "splitList" "join" "sortAlpha" "reverse" "uniq" "compact"
    "toJson" "toPrettyJson" "toRawJson" "fromJson" "toYaml" "fromYaml"
    "toToml" "fromToml" "ternary" "deepCopy" "mustDeepCopy" "typeOf"
    "typeIs" "typeIsLike" "kindOf" "kindIs" "deepEqual" "empty"
    "coalesce" "all" "any" "compact" "mustCompact" "fromJson"
    "mustFromJson" "toJson" "mustToJson" "toPrettyJson" "mustToPrettyJson")
  "List of Helm template keywords and functions.")

(define-hostmode poly-helm-hostmode
  :mode 'yaml-mode
  :keep-in-mode 'host)

(define-innermode poly-helm-template-innermode
  :mode 'go-template-mode
  :head-matcher "{{-?\\s-*"
  :tail-matcher "\\s-*-?}}"
  :head-mode 'host
  :tail-mode 'host
  :keep-in-mode 'host)

(define-minor-mode go-template-mode
  "Minor mode for Go template syntax within Helm templates."
  :lighter " GoTmpl"
  (if go-template-mode
      (progn
        (font-lock-add-keywords
         nil
         `(("{{-?\\|\\s-*-?}}" . 'helm-template-delimiter-face)
           (,(regexp-opt helm-template-keywords 'words) . 'helm-template-keyword-face)
           ("\\$[a-zA-Z_][a-zA-Z0-9_]*" . 'helm-template-variable-face)
           ("\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 'helm-template-action-face)
           ("\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" 1 'helm-template-action-face)))
        (font-lock-flush))
    (font-lock-remove-keywords
     nil
     `(("{{-?\\|\\s-*-?}}" . 'helm-template-delimiter-face)
       (,(regexp-opt helm-template-keywords 'words) . 'helm-template-keyword-face)
       ("\\$[a-zA-Z_][a-zA-Z0-9_]*" . 'helm-template-variable-face)
       ("\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 'helm-template-action-face)
       ("\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" 1 'helm-template-action-face)))
    (font-lock-flush)))

(define-polymode poly-helm-mode
  :hostmode 'poly-helm-hostmode
  :innermodes '(poly-helm-template-innermode)
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-t") 'poly-helm-template-insert-template)
            (define-key map (kbd "C-c C-v") 'poly-helm-template-insert-variable)
            (define-key map (kbd "C-c C-i") 'poly-helm-template-insert-if)
            (define-key map (kbd "C-c C-r") 'poly-helm-template-insert-range)
            map))

(defun poly-helm-template-insert-template ()
  "Insert a template block."
  (interactive)
  (insert "{{ template \"\" . }}"))

(defun poly-helm-template-insert-variable ()
  "Insert a variable reference."
  (interactive)
  (insert "{{ . }}"))

(defun poly-helm-template-insert-if ()
  "Insert an if block."
  (interactive)
  (insert "{{ if  }}\\n\\n{{ end }}")
  (forward-line -1)
  (end-of-line))

(defun poly-helm-template-insert-range ()
  "Insert a range block."
  (interactive)
  (insert "{{ range . }}\\n\\n{{ end }}")
  (forward-line -1)
  (end-of-line))

(defun helm-template-insert-range ()
  "Insert a range block."
  (interactive)
  (insert "{{ range . }}\n\n{{ end }}")
  (forward-line -1)
  (end-of-line))

(defun poly-helm-mode-is-helm-file-p ()
  "Return non-nil if the current buffer's file is a Helm template file."
  (let ((filename (file-name-nondirectory (or buffer-file-name "")))
        (directory-parts (file-name-directory (or buffer-file-name ""))))
    (or (member filename '("values.yaml" "values.yml" "Chart.yaml" "Chart.yml"))
        (and directory-parts
             (let ((dir-components (split-string directory-parts "/")))
               (cl-some (lambda (dir) (member dir poly-helm-mode-template-directories))
                        dir-components))))))

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

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.ya?ml\\'" . (lambda ()
                                 (if (poly-helm-mode-is-helm-file-p)
                                     (poly-helm-mode)
                                   (yaml-mode)))))

(provide 'poly-helm-mode)

;;; poly-helm-mode.el ends here