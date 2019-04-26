# tla-tools

TLA+ tools for Emacs

## Usage

Add the source directory to `load-path` and `M-x load tla-tools`. Or
just eval the buffer.

Execute `M-x tla-tools-error-regexp-add` to add regexps for
compilation mode.

Using [tla-bin](https://github.com/pmer/tla-bin), a TLA+ source file
can be checked with M-x compile, with a compile command of `tlc
<tla-file>` or `pcal <tla-file> && tlc <tla-file>`.

## Bugs

* Doesn't implement much except helping next-error/previous-error.
* No support TLC reports errors just with the module name, the
  filename is guessed to be modulename.tla. Sany errors don't have the
  module name either (makes sense, it's syntax checking the file), so
  it's guessed from the "Parsing file ..." line. Multiple "Parsing
  file" lines could lead the errors to the wrong file.  As a hack,
  there's a super ugly regexp which prevents matching filenames
  beginning with "/private/", which is where the temp files go on my
  installation. Sorry if you need to check /tmp instead, maybe the
  test function offers you some inspiration.
* Error type (warning, error) is not guessed.
* No extra support for "-tool" option (which makes tlc emit
  tool-readable messages.)
* tlc requires a config file, and this package does nothing to help
  generate it.
* It would be nice if it auto reverted the buffer after pcal (which
  rewrites the tla file.)


