FROM ubuntu:15.10

RUN apt-get update && apt-get install -qq -y emacs24-nox

ADD scalastyle_2.11-0.7.0-batch.jar /root/.emacs.d/
ADD scalastyle_config.xml /root/.emacs.d/

ADD init.el /root/.emacs.d/
ADD config /root/.emacs.d/config
ADD lib /root/.emacs.d/lib

# Do initial load to pre-fetch all libraries from melpa and byte compile directory for start up speed
RUN emacs -batch -l /root/.emacs.d/init.el --eval "(byte-recompile-directory \".emacs.d/\" 0)"
