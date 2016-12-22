#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "mirage-net-unix" @@ fun c ->
  Ok [ Pkg.mllib "src/mirage-net-unix.mllib";
       Pkg.test "test/test"
  ]
