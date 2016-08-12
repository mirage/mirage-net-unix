(*
 * Copyright (C) 2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt
open Printf

let run test =
  Lwt_main.run (test ())

let err_connect e =
  let buf = Buffer.create 10 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "didnt connect: %a" Netif.pp_error e;
  failwith (Buffer.contents buf)

let test_open () =
    Netif.connect "tap0" >>= function
    | `Error e -> err_connect e
    | `Ok _t    ->
      printf "connected\n%!";
      Lwt.return_unit

let test_close () =
    Netif.connect "tap1" >>= function
    | `Error e -> err_connect e
    | `Ok t    ->
      printf "connected\n%!";
      Netif.disconnect t >>= function () ->
      printf "disconnected\n%!";
      Lwt.return_unit

let suite : Alcotest.test_case list = [
  "connect", `Quick, (fun () -> run test_open) ;
  "disconnect", `Quick, (fun () -> run test_close)
]

let _ =
  Alcotest.run "mirage-net-unix" [ "tests", suite ]
