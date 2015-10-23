FROM ubuntu:15.10

RUN apt-get update && apt-get install -qq -y emacs24-nox

ADD scalastyle_2.11-0.7.0-batch.jar ~/.emacs.d/
ADD scalastyle_config.xml ~/.emacs.d/

ADD init.el config lib ~/.emacs.d/

# Do initial load to pre-fetch all libraries from melpa and byte compile directory for start up speed
RUN emacs -batch -l ~/.emacs.d/init.el --eval "(byte-recompile-directory \".emacs.d/\" 0)"
