all: test benchmark

test:
	@echo "Testing dynamic checker"
	@echo "========================================="
	@raco test tests/Dyn/*.rkt
	@raco test tests/Dyn/buggy-versions/*.rkt

benchmark:
	@echo "Benchmarking dynamic checker"
	@echo "========================================="
	@echo "## Factorial - unchecked"
	@racket tests/Dyn/benchmarks/unchecked/fact.rkt
	@echo "## Factorial - checked using continuation mark"
	@racket tests/Dyn/benchmarks/cm/fact.rkt
	@echo "## Factorial - checked using imperative updates"
	@racket tests/Dyn/benchmarks/imp/fact.rkt
	@echo "## Sum - unchecked"
	@racket tests/Dyn/benchmarks/unchecked/sum.rkt
	@echo "## Sum - checked using continuation mark"
	@racket tests/Dyn/benchmarks/cm/sum.rkt
	@echo "## Sum - checked using imperative update"
	@racket tests/Dyn/benchmarks/imp/sum.rkt
	@echo "## Mergesort - unchecked"
	@racket tests/Dyn/benchmarks/unchecked/mergesort.rkt
	@echo "## Mergesort - checked using continuation mark"
	@racket tests/Dyn/benchmarks/cm/mergesort.rkt
	@echo "## Mergesort - checked using imperative update"
	@racket tests/Dyn/benchmarks/imp/mergesort.rkt
	@echo "## Interpreted Factorial - unchecked"
	@racket tests/Dyn/benchmarks/unchecked/interp-fact.rkt
	@echo "## Interpreted Factorial - checked using continuation mark"
	@racket tests/Dyn/benchmarks/cm/interp-fact.rkt
	@echo "## Interpreted Factorial - checked using imperative update"
	@racket tests/Dyn/benchmarks/imp/interp-fact.rkt
	@echo "## Interpreted Sum - unchecked"
	@racket tests/Dyn/benchmarks/unchecked/interp-sum.rkt
	@echo "## Interpreted Sum - checked using continuation mark"
	@racket tests/Dyn/benchmarks/cm/interp-sum.rkt
	@echo "## Interpreted Sum - checked using imperative update"
	@racket tests/Dyn/benchmarks/imp/interp-sum.rkt
	@echo "## Interpreted Mergesort - unchecked"
	@racket tests/Dyn/benchmarks/unchecked/interp-mergesort.rkt
	@echo "## Interpreted Mergesort - checked using continuation mark"
	@racket tests/Dyn/benchmarks/cm/interp-mergesort.rkt
	@echo "## Interpreted Mergesort - checked using imperative update"
	@racket tests/Dyn/benchmarks/imp/interp-mergesort.rkt
