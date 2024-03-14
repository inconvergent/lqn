#!/bin/bash
sbcl --quit \
     --eval '(ql:quickload :sb-introspect)'\
     --eval '(load "/path/to/quicklisp/setup.lisp")'\
     --eval '(ql:quickload :lqn)'\ # add more evals to load your own pkgs
     --eval '(save-lisp-and-die "/path/to/lsp.core"
               :executable t :compression nil
               :purify t     :save-runtime-options t)'

# Then make aliases like this:
alias lqn="/path/to/lsp.core --script ~/path/to/lqn/bin/lqn-sh.lisp"
