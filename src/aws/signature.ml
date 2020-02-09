(* Adapted from https://github.com/inhabitedtype/ocaml-aws *) 

open Core

module Hashing = struct
  let _sha256 ?key str =
    let open Nocrypto.Hash.SHA256 in
    let bits = Cstruct.of_string str in
    match key with
    | None -> digest bits
    | Some k -> hmac ~key:(Cstruct.of_string k) bits
  ;;

  let sha256 ?key str = Cstruct.to_string (_sha256 ?key str)

  let sha256_hex ?key str =
    let buf = Buffer.create 65 in
    Cstruct.hexdump_to_buffer buf (_sha256 ?key str);
    Str.(global_replace (regexp "[ \n]")) "" (Buffer.contents buf)
  ;;
end

let encode_query ps =
  ps
  |> List.map ~f:(fun (k, v) ->
         let key = Uri.pct_encode ~component:`Authority k in
         let value =
           match v with
           | [] -> ""
           | [ x ] -> Uri.pct_encode ~component:`Authority x
           | _ -> failwith "AWS query cannot have multiple values for same key"
         in
         key, value)
  |> List.sort ~compare:(fun a b -> String.compare (fst a) (fst b))
  |> List.map ~f:(fun (k, v) -> k ^ "=" ^ v)
  |> String.concat ~sep:"&"
;;

let sign_request ~service ~meth ~uri ?(headers = []) ?(body = "") () =
  let host = Option.value_exn (Uri.host uri) in
  let params = encode_query (Uri.query uri) in
  let sign key msg = Hashing.sha256 ~key msg in
  let get_signature_key key date region service =
    sign (sign (sign (sign ("AWS4" ^ key) date) region) service) "aws4_request"
  in
  let now = Time.now () in
  let amzdate = Time.format now "%Y%m%dT%H%M%SZ" ~zone:Time.Zone.utc in
  let datestamp = Time.format now "%Y%m%d" ~zone:Time.Zone.utc in
  let canonical_uri = Uri.path uri in
  let canonical_querystring = params in
  let canonical_headers =
    [ "host", host; "x-amz-date", amzdate; "x-amz-security-token", Env.session_token ]
  in
  let canonical_headers_str =
    canonical_headers
    |> List.map ~f:(fun (k, v) -> k ^ ":" ^ v)
    |> String.concat ~sep:"\n"
  in
  let signed_headers =
    canonical_headers |> List.map ~f:(fun (k, _) -> k) |> String.concat ~sep:";"
  in
  let payload_hash = Hashing.sha256_hex body in
  let canonical_request =
    String.concat
      ~sep:"\n"
      [ meth
      ; canonical_uri
      ; canonical_querystring
      ; canonical_headers_str ^ "\n"
      ; signed_headers
      ; payload_hash
      ]
  in
  let algorithm = "AWS4-HMAC-SHA256" in
  let credential_scope =
    String.concat ~sep:"/" [ datestamp; Env.region; service; "aws4_request" ]
  in
  let string_to_sign =
    String.concat
      ~sep:"\n"
      [ algorithm; amzdate; credential_scope; Hashing.sha256_hex canonical_request ]
  in
  let signing_key =
    get_signature_key Env.secret_access_key datestamp Env.region service
  in
  let signature = Hashing.sha256_hex ~key:signing_key string_to_sign in
  let authorization_header =
    String.concat
      ~sep:""
      [ algorithm
      ; " "
      ; "Credential="
      ; Env.access_key_id
      ; "/"
      ; credential_scope
      ; ", "
      ; "SignedHeaders="
      ; signed_headers
      ; ", "
      ; "Signature="
      ; signature
      ]
  in
  ("x-amz-date", amzdate)
  :: ("x-amz-security-token", Env.session_token)
  :: ("Authorization", authorization_header)
  :: headers
;;
