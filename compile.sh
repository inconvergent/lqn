#!/bin/bash

set -e
touch ./jqn.asd
time sbcl --quit \
           --eval '(load "jqn.asd")'\
           --eval '(handler-case (time (ql:quickload :jqn :verbose t))
                     (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
  >compile.sh.tmp 2>&1
