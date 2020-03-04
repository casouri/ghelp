echo Byte compiler
emacs -Q --batch -L . -f batch-byte-compile *.el
echo ERT test
emacs -Q --batch -L . -l ghelp-test -f ert-run-tests-batch-and-exit
