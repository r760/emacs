#!/bin/bash

set -x

mkdir -p ~/.emacs.d/packages
find packages -name "*.el" | xargs realpath | xargs -I{} ln -sf {} ~/.emacs.d/packages/.
ln -sf $(realpath .emacs) ~/.emacs
ls -l ~/.emacs ~/.emacs.d/packages | grep "\->"
