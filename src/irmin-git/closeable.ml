(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Content_addressable (S : Irmin.CONTENT_ADDRESSABLE_STORE) = struct
  type 'a t = 'a S.t

  type key = S.key

  type value = S.value

  let closed = ref false

  let check_closed _ = if !closed then raise Irmin.Closed

  let mem t k =
    check_closed t;
    S.mem t k

  let find t k =
    check_closed t;
    S.find t k

  let add t v =
    check_closed t;
    S.add t v

  let unsafe_add t k v =
    check_closed t;
    S.unsafe_add t k v

  let v global_closed t =
    closed := global_closed;
    t
end