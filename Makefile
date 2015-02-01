files = $(wildcard ch*.rkt)
targets = $(patsubst %.rkt,%,$(files))

test:
	@time raco test --direct $(files)

.PHONY: $(targets) all

$(targets):
	@time raco test $@.rkt

run:
	rlwrap racket -i -t $(files)

all: $(targets)
