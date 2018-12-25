## egg-layer

**WARNING**: this is a hack.

`egg-layer` is a tool that generate a Makefile to fetch and install
CHICKEN eggs.  Differently from chicken-install, it uses egg sources
from tarballs available at [1].  `egg-layer` uses some external tools
to fetch, verify and unpack tarballs, namely `wget`, `sha1sum`, `tar`
and `gzip`.

To generate the dependency graph for Makefile rules, `egg-layer` uses
information from an index file [2] (gzip-compressed).  The format of
that file is:

* The first line is the index format version

* the next lines have the following format:

> (EGG-NAME EGG-VERSION TARBALL-SIZE TARBALL-SHA1-SUM EGG-DEPENDENCIES EGG-TEST-DEPENDENCIES)

`egg-layer` downloads and reads the index file, determines the
dependencies among eggs and tasks (fetch-tarball, fetch-checksum,
unpack, install), and generates the Makefile.

Usage example:

    $ ./egg-layer.scm -h
    Usage: egg-layer.scm [<options>] <egg>

    <options>
      -o <dir>:
         output directory (default: current directory)
    $ ./egg-layer.scm -o tmp spiffy
    $ cd tmp
    $ make help
    all: install the egg given as argument to egg-layer and its dependencies
    fetch: fetch all egg tarballs
    unpack: unpack all egg tarballs
    clean: remove all egg tarballs, checksum files and egg directories
    $ make

* [1] https://code.call-cc.org/egg-tarballs/
* [2] https://code.call-cc.org/egg-tarballs/CHICKEN-MAJOR-VERSION/index.gz
      (where `CHICKEN-MAJOR-VERSION` can be currently `4` or `5`)


### Why

* Use egg tarballs
  * smaller size
  * integrity verification (HTTPS + checksum)

* Possibility of using the parallel execution feature of make to fetch
  and install eggs


### Bugs, limitations and assorted notes

* It has been very minimally tested on Linux with GNU make.

* No concern regarding portability.

* It uses `chicken-install` as build system for eggs (but not to fetch
  and verify integrity of sources).

* The generated Makefile assumes that `chicken-install` is somewhere
  in `$PATH`.  It doesn't hardcode the full path to the
  `chicken-install` executable in the Makefile (maybe it should).

* When generating the Makefile, `egg-layer` skips eggs already
  installed.  The state of the repo might be different when the
  Makefile is actually processed.

* It only admits a single egg as argument.

* It completely ignores egg versions.

* It completely ignore egg tests.

* It hopefully supports both CHICKEN 4 and 5.

* Due to the way `egg-layer` uses `chicken-install`
  (`cd egg-dir && chicken-install`), it ends up not using the
  egg compilation cache.

* It's code is disgusting.

* It is just very rough proof-of-concept.  It is incomplete and
  probably full of bugs.
