drewmacs2
=========

A substantial simplification of my old .emacs file setup that uses
the MELPA package manager.  Still a work in progress, but should
have most of the features of drewmacs.  Emacs 24 is supported,
and I have made no effort to support earlier versions

### Some notes regarding Jedi

To use Jedi with this, you need to install the python dependencies
explicitly.  Eventually that should be done via some sort of makefile,
but eh.  Also this implementation of Jedi doesn't use its own virtualenv.
Instead, it points to the currently active system python, and adds the
current project root, if any.
