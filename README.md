# Hiqmail v2 #

## Directory Structure ##

- docs/ project documentation
  - make-doc
    Make project documentation, run without arguments to see help.
  - hiqXXXXX/
    Application documentation (generated with 'make-doc').
  - index.html
    Project documentation index (generated with 'make-doc').

- hiqXXXXX/
  - deps/
    Needed dependencies (usually downloaded).
  - ebin/
    Erlang object files.
  - include/
    Include files (.hrl).
  - priv/
    Application specific files, accessed by calling
    code:priv_dir/1.
  - src/
    Source code.
  - erlang.mk
    Make file.
  - relx.config
    Release config file.
  - sys.config
    App config file.

## Building Applications ##

Type:
```
cd hiqXXXX
make
./relx -l ../ -o release
./release/bin/hiqXXXX
```
