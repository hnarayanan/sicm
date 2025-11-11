# Working Through Structure and Interpretation of Classical Mechanics

This repository contains some notes on my progress through [Structure
and Interpretation of Classical Mechanics][sicm] (Second Edition).
This book lies at the intersection of things that matter very much to
me: mechanics, mathematics and computation.

The crew behind this book (and the corresponding code) are a curious
bunch and they have fun exploring their curiosities. They are slowly
becoming my spiritual guides.

These are mostly notes for myself, but it's great if you want to
follow along with:

- [My console logs as I work through the book][gh-sicm-working]
- [My solutions to the exercises in the book][gh-sicm-exercises]

Feel free to [get in touch][hn-email] if you'd like to talk about this
topic or study together.

## Background Material

There are a handful of ways you can access the book:

- You can buy a physical copy of this book [at its official store
page][sicm],
- Read [the freely-available official HTML version][sicm-html], or
- Read [a beautiful unofficial version][sicm-unofficial-html] online.

There is also an MIT OCW course corresponding to it:

- [Classical Mechanics: A Computational Approach by Prof. Gerald
  Sussman and Jack Wisdom][cm-course-sussman-wisdom]

The material in this course, and hence the numbering used in the
assignments, corresponds to the first edition of the book.

The programming language used in this book and the accompanying
library ([Scheme Mechanics or Scmutils][scmutils]) is called
[Scheme][mit-scheme]. If you are interested in learning about it, the
following course (and [related book][sicp]) is excellent:

- [Structure And Interpretation Of Computer Programs by Hal Abelson
   and Gerald Jay Sussman][programming-course-abelson-sussman]

In addition to this, the following courses are also really
interesting:

- [Classical Mechanics by Leonard Susskind][cm-course-susskind]
- [Classical Physics by Venkataraman Balakrishnan][physics-course-balakrishnan]
- [Fundamentals of Physics with Ramamurti Shankar][physics-course-shankar]
- [Nonlinear Dynamics and Chaos by Steven Strogatz][physics-course-strogatz]

## Following Along

### Base setup for macOS on Apple Silicon

I happen to work on an Apple Silicon Mac, and part of this repository
is a collection of notes and hacks needed to get [MIT
Scheme][mit-scheme] and [Scmutils][scmutils] working nicely on it.

1. Install [XQuartz][xquartz]. This used to come bundled with Macs
   previously, but now needs to be installed by hand. You will need
   this for graphical output.

   Simply [download the installer][xquartz-installer] (2.8.5 at the
   time of writing) and run it to install.

2. Fetch, unpack and patch recent source code for MIT Scheme. At the
   time of writing, this is version 12.1. For some technical reasons,
   there is no native-code support for Apple Silicon, and you need to
   fetch the SVM1 binaries which work (albeit a little slowly).

   ````
   curl -O https://ftp.gnu.org/gnu/mit-scheme/stable.pkg/12.1/mit-scheme-12.1-svm1-64le.tar.gz
   tar -xzf mit-scheme-12.1-svm1-64le.tar.gz
   cd mit-scheme-12.1
   curl -O https://raw.githubusercontent.com/hnarayanan/sicm/main/patches/mit-scheme-12.1.patch
   patch -p1 < mit-scheme-12.1.patch
   ````

3. Build and install this patched MIT Scheme. You will want to change
   `/path/to/install/scheme/` below to something that makes sense for
   you. Just keep it in mind in subsequent steps.

   ````
   ./configure --prefix=/path/to/install/scheme/
   make
   make install
   ````
4. Adjust some environment variables in `~/.profile` to let your shell
   know about this installation.

   ````
   export DISPLAY=:0
   export PATH=/path/to/install/scheme/bin:${PATH}
   export MITSCHEME_LIBRARY_PATH=/path/to/install/scheme/lib/mit-scheme-svm1-64le-12.1
   ````

   You can open a new shell and test your installation out. Hooray!

   ````
   mit-scheme
   ````

5. The final step is to fetch and install a recent
   [Scmutils][scmutils] (20230902 at the time of writing).

   ````
   curl -O https://groups.csail.mit.edu/mac/users/gjs/6946/mechanics-system-installation/svm/scmutils-20230902.tar.gz
   tar -xzf scmutils-20230902.tar.gz
   cd scmutils-20230902
   ./install.sh
   ````

   This ends up installing a script (called `mechanics.sh`) that
   starts Scheme and loads Scmutils as it does so. Since it is put in
   the same path that the `mit-scheme` binary was installed in, you
   can just run it from your shell.

   ````
   mechanics.sh
   ````

   This greets you with the following: *Notice the modules loaded on
   the last line.* If you see these, it means you have it all working.

   ````
   MIT/GNU Scheme running under OS X
   Type `^C' (control-C) followed by `H' to obtain information about interrupts.

   Copyright (C) 2022 Massachusetts Institute of Technology
   This is free software; see the source for copying conditions. There is NO warranty; not even for MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.

   Image saved on Saturday September 2, 2023 at 2:29:59 AM
     Release 12.1 || SF || CREF || LIAR/svm1 || SOS || XML || Edwin || X11 || X11-Screen || ScmUtils
   ````

### Integration with GNU Emacs

I also happen to use [GNU Emacs][gnu-emacs], so here is some bonus
material.

If you add the following blocks to your Emacs configuration, they
install and configure a package called [Geiser][emacs-geiser] that
provides a nicer [REPL][wiki-repl] for Scheme with scmutils working
within Emacs. This makes hacking along with this book even more fun.

````
(use-package geiser
  :ensure t
  :defer t
  :custom
  (geiser-active-implementations '(mit))
  :config
  (setenv "DISPLAY" ":0"))

(use-package geiser-mit
  :ensure t
  :defer t
  :custom
  (geiser-mit-binary "/path/to/install/scheme/bin/mit-scheme")
  :config
  (setenv "MITSCHEME_HEAP_SIZE" "100000")
  (setenv "MITSCHEME_BAND" "mechanics.com")
  (setenv "MITSCHEME_LIBRARY_PATH" "/path/to/install/scheme/lib/mit-scheme-svm1-64le-12.1"))
````

Once you've adjusted the paths and added this to your setup, running
`M-x geiser` loads the REPL.

If you are interested in more of my Emacs environment, I [share my
entire configuration online][hn-dotemacs].

## License

This learning repository is licensed under the [Creative Commons
Attribution 4.0 International License][license-cc-by].

[cm-course-susskind]: http://theoreticalminimum.com/courses/classical-mechanics/2011/fall
[cm-course-sussman-wisdom]: https://ocw.mit.edu/courses/12-620j-classical-mechanics-a-computational-approach-fall-2008/
[emacs-geiser]: https://www.nongnu.org/geiser/
[gh-sicm-exercises]: https://github.com/hnarayanan/sicm/tree/main/exercises
[gh-sicm-working]: https://github.com/hnarayanan/sicm/tree/main/working
[gnu-emacs]: https://www.gnu.org/software/emacs/
[hn-dotemacs]: https://github.com/hnarayanan/dotemacs
[hn-email]: mailto:mail@harishnarayanan.org
[license-cc-by]: https://creativecommons.org/licenses/by/4.0/
[mit-scheme]: https://www.gnu.org/software/mit-scheme/
[physics-course-balakrishnan]: https://www.youtube.com/playlist?list=PL5E4E56893588CBA8
[physics-course-shankar]: https://www.youtube.com/playlist?list=PLFE3074A4CB751B2B
[physics-course-strogatz]: https://www.youtube.com/playlist?list=PLbN57C5Zdl6j_qJA-pARJnKsmROzPnO9V
[programming-course-abelson-sussman]: http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/
[scmutils]: https://groups.csail.mit.edu/mac/users/gjs/6946/installation.html
[sicm-html]: https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/9579/sicm_edition_2.zip/book.html
[sicm-unofficial-html]: https://tgvaughan.github.io/sicm/
[sicm]: https://mitpress.mit.edu/9780262028967/structure-and-interpretation-of-classical-mechanics/
[sicp]: https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/index.html
[wiki-repl]: https://en.wikipedia.org/wiki/Read–eval–print_loop
[xquartz-installer]: https://github.com/XQuartz/XQuartz/releases/download/XQuartz-2.8.5/XQuartz-2.8.5.pkg
[xquartz]: https://www.xquartz.org
