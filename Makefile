EMACS ?= emacs

.PHONY: test

test:
	@if ! command -v cask > /dev/null; then \
		echo "Cask is not installed. Please install it from https://github.com/cask/cask"; \
		exit 1; \
	fi
	cask install
	cask exec $(EMACS) -batch -L . -l ert -l helm-mode-tests.el -f ert-run-tests-batch-and-exit
