#!/bin/bash

set -x

find . -name "*.el" | xargs realpath | xargs -I{} ln -sf {} ~/.emacs.d/.
ln -sf $(realpath emacs.org) ~/.emacs.d/.
ln -sf $(realpath snippets) ~/.emacs.d/.
ls -l ~/.emacs.d | grep "\->"
