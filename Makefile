testein-default:
	./testein.py --clean-elc --load-ert

testein-24:
	./testein.py --clean-elc -e emacs-snapshot

interactive-testein-default:
	./testein.py --clean-elc --load-ert --no-batch

interactive-testein-24:
	./testein.py --clean-elc -e emacs-snapshot --no-batch
