# Working Through Structure and Interpretation of Classical Mechanics

This repository contains some notes on my progress through [Structure
and Interpretation of Classical Mechanics][sicm]. This book lies at
the intersection of things that matter very much to me: mechanics,
mathematics and computation.

The crew behind this book (and the corresponding code) are a curious
bunch and they have fun exploring their curiosities. They are slowly
becoming my spiritual guides.

These are mostly notes for myself, but it's great if you want to
follow along.

## Background Material

You can buy a physical copy of this book [at its official store
page][sicm], or you can read [a beautiful rendition of the HTML
version online][sicm-html]. There is also an MIT OCW course
corresponding to it:

- [Classical Mechanics: A Computational Approach by Prof. Gerald
  Sussman and Jack Wisdom][cm-course-sussman-wisdom]

The language used in this book and the accompanying library ([Scheme
Mechanics or Scmutils][scmutils]) is called [Scheme][mit-scheme]. If
you are interested in learning about it, the following course (and
[related book][sicp]) is excellent:

-  [Structure And Interpretation Of Computer Programs by Hal Abelson
   and Gerald Jay Sussman][programming-course-abelson-sussman]

In addition to this, the following courses are also really
interesting:

- [Classical Mechanics by Leonard Susskind][cm-course-susskind]
- [Classical Physics by Venkataraman Balakrishnan][physics-course-balakrishnan]
- [Fundamentals of Physics with Ramamurti Shankar][physics-course-shankar]

## Following Along

I happen to work on an Apple Silicon Mac, and part of this repository
is a collection of notes and hacks needed to get [MIT
Scheme][mit-scheme] and [Scmutils][scmutils] working nicely on it. I
also happen to use [GNU Emacs][gnu-emacs], so there will be some bonus
material on how you can get a nice [REPL][wiki-repl] working in Emacs.

1. Install [XQuartz][xquartz]. This used to come bundled with Macs
   previously, but now needs to be installed by hand. You will need
   this for graphical output.

   Simply [download the installer][xquartz-installer] (2.8.5 at the time of writing) and
   run it to install.

2. Fetch, unpack and patch recent source code for MIT Scheme. At the
   time of writing, this is version 12.1. For some technical reasons,
   there is no native-code support for Apple Silicon, and you need to
   fetch the SVM1 binaries which work (a little slow).

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

5. The final step is to fetch and install a recent Scmutils (20230902 at time of writing).

   ````
   curl -O https://groups.csail.mit.edu/mac/users/gjs/6946/mechanics-system-installation/svm/scmutils-20230902.tar.gz
   tar -xzf scmutils-20230902.tar.gz
   cd scmutils-20230902
   ./install.sh
   ````

   Now you can use a specialised script that starts Scheme and loads
   Scmutils as it does so. It is already in the same path that the
   `mit-scheme` binary was installed in. So you can just call it in
   your shell.

   ````
   mechanics.sh
   ````

   This is not a very nice name for a file, so you can call it
   whatever else you want.



## License

This tutorial is licensed under the [Creative Commons Attribution 4.0
International License][license-cc-by].

[cm-course-susskind]: http://theoreticalminimum.com/courses/classical-mechanics/2011/fall
[cm-course-sussman-wisdom]: https://ocw.mit.edu/courses/12-620j-classical-mechanics-a-computational-approach-fall-2008/
[gnu-emacs]: https://www.gnu.org/software/emacs/
[license-cc-by]: https://creativecommons.org/licenses/by/4.0/
[mit-scheme]: https://www.gnu.org/software/mit-scheme/
[physics-course-balakrishnan]: https://www.youtube.com/playlist?list=PL5E4E56893588CBA8
[physics-course-shankar]: https://www.youtube.com/playlist?list=PLFE3074A4CB751B2B
[programming-course-abelson-sussman]: http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/
[scmutils]: https://groups.csail.mit.edu/mac/users/gjs/6946/installation.html
[sicm-html]: https://tgvaughan.github.io/sicm/
[sicm]: https://mitpress.mit.edu/9780262028967/structure-and-interpretation-of-classical-mechanics/
[sicp]: https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/index.html
[wiki-repl]: https://en.wikipedia.org/wiki/Read–eval–print_loop
[xquartz-installer]: https://github.com/XQuartz/XQuartz/releases/download/XQuartz-2.8.5/XQuartz-2.8.5.pkg
[xquartz]: https://www.xquartz.org
