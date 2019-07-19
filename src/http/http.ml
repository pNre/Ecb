open Async
open Core
open Cohttp
open Cohttp_async

type error =
  | Request of exn
  | Response of Uri.t * Response.t * Body.t

let string_of_body = Cohttp_async.Body.to_string

let string_of_error = function
  | Request exn -> Exn.to_string exn
  | Response (uri, response, _) ->
    let code = Code.code_of_status response.status in
    let headers = Header.to_string response.headers in
    let uri = Uri.to_string uri in
    sprintf "uri = %s, status = %d, headers = %s" uri code headers
;;

let perform_request uri req max_redirects =
  let rec perform uri redirects_left =
    if redirects_left = 0
    then Deferred.return (Error (Request (Failure "Too many redirects")))
    else
      let open Cohttp.Response in
      let response =
        try_with (fun () -> req uri) >>| Result.map_error ~f:(fun e -> Request e)
      in
      response
      >>=? function
      | ({ status; _ } as response), body
        when Code.is_success (Code.code_of_status status) ->
        (response, body) |> Result.return |> Deferred.return
      | ({ status; headers; _ } as response), body
        when Code.is_redirection (Code.code_of_status status) ->
        (match Header.get_location headers with
        | Some redirect_uri -> perform redirect_uri (redirects_left - 1)
        | None -> Response (uri, response, body) |> Result.fail |> Deferred.return)
      | response, body ->
        Response (uri, response, body) |> Result.fail |> Deferred.return
  in
  perform uri max_redirects
;;

let request http_method uri ?(http_headers = []) ?(body = None) ?(max_redirects = 3) () =
  perform_request
    uri
    (Client.call
       ~headers:(Header.of_list http_headers)
       ~body:(Option.value_map body ~default:`Empty ~f:Body.of_string)
       http_method)
    max_redirects
;;

let post_form uri ?(http_headers = []) ~params ?(max_redirects = 3) () =
  perform_request
    uri
    (Client.post_form ~headers:(Header.of_list http_headers) ~params)
    max_redirects
;;
