#!/bin/bash

set -e
echo '#### running SBCL tests:'
touch ./lqn.asd
time sbcl --quit \
     --eval '(ql:quickload :prove)'\
     --eval '(handler-case (ql:quickload :lqn :verbose nil)
                           (error (c) (format t "STAGE1FAIL: ~a" c)
                                      (uiop:quit 2)))'\
     --eval '(handler-case (asdf:test-system :lqn)
                           (error (c) (format t "STAGE2FAIL: ~a" c)
                                      (uiop:quit 3)))'

cd bin ; time ./test-sh.sh
cd .. ; touch ./lqn.asd

