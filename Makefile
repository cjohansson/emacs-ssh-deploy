EMACS = emacs
ifdef emacs
	EMACS = $(emacs)
endif

EMACS_CMD := $(EMACS) -Q -batch -L .

.PHONY: tests
tests:
	$(EMACS_CMD) -l ssh-deploy-test.el

