## egg-layer

**WARNING**: this is a hack.

`egg-layer` is a tool that generate a Makefile to fetch and install
CHICKEN eggs.  Differently from chicken-install, it uses egg sources
from tarballs available at [1].  `egg-layer` relies on external tools
to fetch, verify and unpack tarballs.  The default configuration uses
`wget`, `sha1sum`, `tar` and `gzip` (parameters which use those
commands are configurable -- see `egg-layer-params.scm`).

To generate the dependency graph for Makefile rules, `egg-layer` uses
information from an index file [2] (gzip-compressed).  The format of
that file is:

* The first line is the index format version

* the next lines have the following format:

> (EGG-NAME EGG-VERSION TARBALL-SIZE TARBALL-SHA1-SUM EGG-DEPENDENCIES EGG-TEST-DEPENDENCIES)

`egg-layer` downloads and reads the index file, determines the
dependencies among eggs and tasks (fetch-tarball, fetch-checksum,
unpack, install), and generates the Makefile.

Usage:

```
$ egg-layer -h
Usage: egg-layer [<options>] [<egg> ...]

If no egg is provided as argument, the current directory is assumed to be
the source directory of an egg.

<options>
  --action|-a <action>:
    Action to be executed (default: all).  Available actions:
    * all: install egg.
    * fetch: fetch egg and its dependencies.
    * none: only generate the Makefile.
    * unpack: fetch egg and its dependencies and unpack them.

  --config-file|-c <file>:
    Specify a configuration file alternative to the default one
    ($HOME/.egglayer.conf).

  --force-dependencies:
    Force processing actions for dependencies.  Without this option,
    actions for dependencies which are already installed will be skipped.

  --keep-output-directory|-k:
    By default, the output directory containing the Makefile generated by
    this program will be removed (unless --output-dir is provided).  With
    this option the the output directory is not removed.

  --output-dir|-o <dir>:
    Output directory (default: temporary directory with a random name)

  --parallel-tasks|-j <number>:
    The value of this parameter maps to the value of the -j parameter
    for make.  If given a negative value, -j for make will be given no
    value.

  --verbose:
    Print more information to the output.
```

* [1] https://code.call-cc.org/egg-tarballs/
* [2] https://code.call-cc.org/egg-tarballs/5/index.gz


### Why

* Use egg tarballs
  * smaller size
  * integrity verification (HTTPS + checksum)

* Possibility of using the parallel execution feature of make to fetch
  and install eggs


### Configuration

`egg-layer` loads `$HOME/.egg-layer.conf` if it exists.  See
`egg-layer-params.scm` for the possible configuration parameters.

Here's an example, which sets the parallelization option to the number
of CPUs on a Linux system:

```
(import (chicken process))
(import egg-layer-params)

(parallel-tasks (with-input-from-pipe "nproc" read)
```

Alternatively, you can use the `--config-file` command line parameter
to specify another file (in this case, `$HOME/.egg-layer.conf` will
not be loaded).

Command line options clobber settings make in configuration files.


### Bugs, limitations and assorted notes

* It has been very minimally tested on Linux with GNU make.

* No concern regarding portability.

* It uses `chicken-install` as build system for eggs (but not to fetch
  and verify integrity of sources).

* It completely ignores egg versions.

* It completely ignore egg tests.

* It supports only CHICKEN 5.

* Due to the way `egg-layer` uses `chicken-install`
  (`cd egg-dir && chicken-install`), it ends up not using the
  egg compilation cache.

* Its code is disgusting.

* It is just a very rough proof-of-concept.  It is incomplete and
  probably full of bugs.
