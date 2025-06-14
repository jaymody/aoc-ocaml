Advent of code solutions in OCaml.

Use `OCaml 5.3.0` and [Base](https://github.com/janestreet/base):
```bash
opam switch create 5.3.0
opam switch 5.3.0
opam install ocaml-lsp-server ocamlformat utop
opam install base core yojson ppx_jane ppx_expect
```

Download your puzzle data from advent of code. You'll need to get your session
cookie from your browser:
```bash
cd data
./download.sh
```

Build, run tests, promote files (if test output changes), and run the executable:
```bash
dune build
dune runtest
dune promotion apply
dune exec bin/aoc.exe y2015_day1_part1
```
