# mmm-helm-mode Tasks

## test

> Run the test suite. This requires Cask to be installed.

```sh
#!/usr/bin/env bash

set -e

# Use EMACS environment variable if set, otherwise default to "emacs"
EMACS=${EMACS:-emacs}

if ! command -v cask > /dev/null; then
  echo "Cask is not installed. Please install it from https://github.com/cask/cask"
  exit 1
fi

cask install
cask exec "$EMACS" -batch -L . -l ert -l helm-mode-tests.el -f ert-run-tests-batch-and-exit
```
