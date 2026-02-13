Project Overview

This project is a quick, fun, real time terminal graphics demo written in OCaml using Notty_unix, rendering an animated 3D cube directly in the terminal. It implements a recursive frame loop, time based animation, and non blocking keyboard input handling for interaction and clean exit. I used this to learn a bit about OCaml, its a pretty cool language :)

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
https://github.com/user-attachments/assets/e7246a22-9dcb-4544-b6f5-92d719a7d4f0

