# helm-mode

An Emacs major mode for editing Helm templates with syntax highlighting for both YAML and Go template syntax.

## Features

- **Mixed syntax highlighting**: Combines YAML mode with Go template syntax
- **Smart file detection**: Automatically activates for `.yaml`/`.yml` files in `templates/` or `charts/` directories
- **Template-specific faces**: Custom highlighting for template delimiters, keywords, variables, and functions
- **Convenient keybindings**: Quick insertion of common template constructs
- **Built on Polymode**: Uses the modern and actively maintained Polymode framework

## Installation

### From MELPA (Recommended)

```elisp
(use-package helm-mode
  :ensure t)
```

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/helm-mode.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/helm-mode")
   (require 'helm-mode)
   ```

## Dependencies

- Emacs 25.1+
- [polymode](https://github.com/polymode/polymode) 0.2.2+
- [yaml-mode](https://github.com/yoshiki/yaml-mode) 0.0.15+

## Usage

The mode automatically activates for `.yaml` and `.yml` files located in directories named `templates` or `charts`. You can customize this behavior:

```elisp
(setq helm-mode-template-directories '("templates" "charts" "my-templates"))
```

### Keybindings

| Key         | Command                        | Description                    |
|-------------|--------------------------------|--------------------------------|
| `C-c C-t`   | `helm-template-insert-template`| Insert template block          |
| `C-c C-v`   | `helm-template-insert-variable`| Insert variable reference      |
| `C-c C-i`   | `helm-template-insert-if`      | Insert if block                |
| `C-c C-r`   | `helm-template-insert-range`   | Insert range block             |

### Syntax Highlighting

The mode provides specialized highlighting for:

- **Template delimiters**: `{{` and `}}` (including whitespace control `{{-` and `-}}`)
- **Keywords**: `if`, `else`, `end`, `range`, `with`, `template`, `define`, etc.
- **Variables**: `$variable`, `.Values.something`
- **Functions**: Built-in Helm template functions like `include`, `required`, `default`

## Examples

Here's how a typical Helm template looks with syntax highlighting:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: {{ include "mychart.fullname" . }}
  labels:
    {{- include "mychart.labels" . | nindent 4 }}
spec:
  {{- if not .Values.autoscaling.enabled }}
  replicas: {{ .Values.replicaCount }}
  {{- end }}
  selector:
    matchLabels:
      {{- include "mychart.selectorLabels" . | nindent 6 }}
  template:
    metadata:
      labels:
        {{- include "mychart.selectorLabels" . | nindent 8 }}
    spec:
      containers:
      - name: {{ .Chart.Name }}
        image: "{{ .Values.image.repository }}:{{ .Values.image.tag | default .Chart.AppVersion }}"
        ports:
        - name: http
          containerPort: {{ .Values.service.port }}
          protocol: TCP
```

## Customization

### Faces

You can customize the appearance of template syntax:

```elisp
(custom-set-faces
 '(helm-template-delimiter-face ((t (:foreground "red" :weight bold))))
 '(helm-template-keyword-face ((t (:foreground "blue" :weight bold))))
 '(helm-template-variable-face ((t (:foreground "green"))))
 '(helm-template-action-face ((t (:foreground "purple")))))
```

### Template Directories

Customize which directories trigger helm-mode:

```elisp
(setq helm-mode-template-directories '("templates" "charts" "k8s-templates"))
```

## Testing

To run the tests, you will need to have Emacs and [Cask](https://github.com/cask/cask) installed. Then, from the root of the repository, run:

```bash
make test
```

This will install the dependencies and run the ERT tests in batch mode, printing the results to the console.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.