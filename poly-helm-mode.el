;;; poly-helm-mode.el --- Major mode for Helm templates -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Rami Chowdhury <rami.chowdhury@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (polymode "0.2.2") (yaml-mode "0.0.15"))
;; Keywords: languages, helm, kubernetes, yaml, templates
;; URL: https://github.com/necaris/poly-helm-mode
;; License: GPLv3

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
;; YAML syntax with Go template directives (`{{ }}' blocks).
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

;;; Code:

(require 'polymode)
(require 'yaml-mode)

(defgroup poly-helm nil
  "Major mode for editing Helm templates."
  :group 'languages
  :prefix "poly-helm-mode-")

(defcustom poly-helm-template-directories '("templates" "charts")
  "List of directory names that indicate Helm template files."
  :type '(repeat string)
  :group 'poly-helm-mode)

(defface poly-helm-template-delimiter-face
  '((t :inherit font-lock-bracket-face :weight bold))
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

(defconst poly-helm-go-template-font-lock-keywords
  `(
    ("{{-?\\|-?}}"
     (0 'poly-helm-template-delimiter-face))
    (,(concat "\\b" (regexp-opt poly-helm-template-keywords 't) "\\b")
     (1 'poly-helm-template-keyword-face))
                                        ; ("\\$[a-zA-Z_][a-zA-Z0-9_]*"
                                        ;  (0 'poly-helm-template-variable-face))
                                        ; ("\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
                                        ;  (1 'poly-helm-template-action-face))
                                        ; ("\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*("
                                        ;  (1 'poly-helm-template-action-face)))
    )
  "Font lock keywords for `poly-helm-go-template-mode'.")

(define-derived-mode poly-helm-go-template-mode prog-mode "GoTmpl"
  "Minor mode for Go template syntax within Helm templates."
  (setq-local font-lock-defaults '(poly-helm-go-template-font-lock-keywords t)))

(define-hostmode poly-helm-hostmode
  :mode 'yaml-mode
  :keep-in-mode 'host)

(define-innermode poly-helm-template-innermode
  :mode 'poly-helm-go-template-mode
  :head-matcher "{{-?"
  :tail-matcher "-?}}"
  :head-mode 'body
  :tail-mode 'body
  :keep-in-mode 'host)

(define-polymode poly-helm-mode nil
  "Mode for Helm templates, combining YAML with Go templates."
  :hostmode 'poly-yaml-hostmode
  :innermodes '(poly-helm-template-innermode)
  (turn-on-font-lock)
  (font-lock-ensure))

(defun poly-helm-mode-is-helm-file-p ()
  "Return non-nil if the current buffer's file is a Helm template file."
  (let ((filename (file-name-nondirectory (or buffer-file-name "")))
        (directory-parts (file-name-directory (or buffer-file-name ""))))
    (and (or (member filename '("values.yaml" "values.yml" "Chart.yaml" "Chart.yml"))
             (and directory-parts
                  (let ((dir-components (split-string directory-parts "/")))
                    (cl-some (lambda (dir) (member dir poly-helm-template-directories))
                             dir-components))))
         t)))

(defun poly-helm-or-yaml-mode ()
  "Enable poly-helm-mode if it's a Helm template file, otherwise YAML mode."
  (if (poly-helm-mode-is-helm-file-p)
      (poly-helm-mode)
    (yaml-mode)))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.ya?ml\\'" . poly-helm-or-yaml-mode))

(provide 'poly-helm-mode)

;;; poly-helm-mode.el ends here
