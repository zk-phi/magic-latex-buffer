# SPDX-License-Identifier: GPL-2.0-or-later

EMACS ?= emacs
FIXTURE := test/fixtures/large-document.tex

.PHONY: test benchmark fixture

test: $(FIXTURE)
	$(EMACS) -Q --batch -L . -l ert \
		-l test/magic-latex-buffer-test.el \
		-f ert-run-tests-batch-and-exit

benchmark: $(FIXTURE)
	MLB_BENCH_ITERATIONS=10 $(EMACS) -Q --batch -L . \
		-l test/benchmark.el -- $(FIXTURE)

fixture:
	$(EMACS) -Q --batch -l test/generate-large-fixture.el -- $(FIXTURE) 128

$(FIXTURE): test/generate-large-fixture.el
	$(MAKE) fixture
