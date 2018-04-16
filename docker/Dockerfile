FROM lokedhs/sbcl-quicklisp:latest

RUN apt-get install -y git
WORKDIR /root/quicklisp/local-projects
RUN git clone git@github.com:own-pt/cl-conllu.git

RUN sbcl --quit --eval "(progn (ql:quickload :cl-conllu))"

WORKDIR /home/
ENTRYPOINT ["sbcl"]
