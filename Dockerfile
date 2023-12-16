# This image is only intended to run the tests

FROM ubuntu:22.04 AS base

RUN apt-get -qq update &&\
    apt-get -qq install -y sbcl curl gcc git

WORKDIR /opt
RUN curl -s 'https://beta.quicklisp.org/quicklisp.lisp' > /opt/quicklisp.lisp
RUN sbcl --noinform --load /opt/quicklisp.lisp\
         --eval '(quicklisp-quickstart:install :path "/opt/quicklisp")'\
         --eval '(sb-ext:quit)'

RUN echo '(load "/opt/quicklisp/setup.lisp")' > /root/.sbclrc
RUN mkdir -p quicklisp
RUN mkdir -p /opt/data
RUN apt-get -qq remove curl -y &&\
    apt-get -qq autoremove -y &&\
    apt-get -qq autoclean -y

from base AS build

WORKDIR /opt
ADD src quicklisp/local-projects/jqn/src
ADD test quicklisp/local-projects/jqn/test
ADD jqn.asd quicklisp/local-projects/jqn
ADD run-tests.sh quicklisp/local-projects/jqn/run-tests.sh
RUN mkdir -p ~/quicklisp/ && ln -s  /opt/quicklisp/setup.lisp ~/quicklisp/setup.lisp

WORKDIR /opt/quicklisp/local-projects/jqn/

CMD ["bash", "./run-tests.sh"]
