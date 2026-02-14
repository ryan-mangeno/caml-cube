Project Overview

This project is a quick, fun, real time terminal graphics demo written in OCaml using Notty_unix, with a small custom obj loader directly in the terminal. It implements a recursive frame loop, time based animation, and non blocking keyboard input handling for interaction and clean exit. I used this to learn a bit about OCaml, its a pretty cool language :)

Quick Setup

Install dependencies:
```bash
opam install notty notty.unix
```

Then build and run with dune:
```bash
dune build
dune exec ./main.exe
```

https://github.com/user-attachments/assets/0d9011a1-9111-4751-9ec0-0fb992f872d4


