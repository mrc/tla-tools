# tla-tools

TLA+ tools for Emacs

### tla-pcal-mode

`tla-pcal-mode` is a mixed mode for editing TLA+ and PlusCal source
files. It depends on [polymode](https://polymode.github.io/).

It supports the [p-syntax](https://lamport.azurewebsites.net/tla/p-manual.pdf)
for PlusCal, not the c-syntax.

A basic TLA+ [auto-insert](https://www.gnu.org/software/emacs/manual/html_node/autotype/Autoinserting.html) template is provided.

## Usage

Press `C-c C-c` for a menu of commands.

The command `tla-create-tlc-config-file` (`C-c C-c c`) creates an empty
configuration file for TLC, with placeholders for constants, the INIT and NEXT
equation, and any invariants.

`C-c C-c m` starts the TLC model checker with the last selected configuration
file.  To select a configuration file before running TLC, press `C-c C-c -m m`.
Press `-d` before `m` to toggle deadlock checking.

`C-c C-c t` translates PlusCal and selects the created configuration file.

`C-c C-c p` creates a PDF version of the current file.

The menu commands expect [tla-bin](https://github.com/pmer/tla-bin) to be
installed and `pcal`, `tlc` and `tlatex` to be in the path.

### Compilation-mode helper
Add the source directory to `load-path` and `M-x load tla-tools`. Or
just eval the buffer.

Execute `M-x tla-tools-error-regexp-add` to add regexps for
compilation mode.

Using [tla-bin](https://github.com/pmer/tla-bin), a TLA+ source file
can be checked with M-x compile, with a compile command of `tlc
<tla-file>` or `pcal <tla-file> && tlc <tla-file>`.

### Bugs

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
* It would be nice if it auto reverted the buffer after pcal (which
  rewrites the tla file.) Until something nicer is done, I'm using
  `auto-revert-mode`. Alternatively check out
  [tlaplus-cli-template](https://github.com/owickstrom/tlaplus-cli-template)
  which keeps your source clean and puts the pcal translations into a
  `target/` directory.

### Running pcal and tlc

* [tla-bin](https://github.com/pmer/tla-bin)
* [TLC command line options](https://lamport.azurewebsites.net/tla/tlc-options.html?back-link=tools.html)
* [tlaplus-cli-template](https://github.com/owickstrom/tlaplus-cli-template)
