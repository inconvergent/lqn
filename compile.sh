#!/bin/bash

set -e
touch ./lqn.asd
time sbcl --quit \
           --eval '(load "lqn.asd")'\
           --eval '(handler-case (time (ql:quickload :lqn :verbose t))
                     (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
  >compile.sh.tmp 2>&1
