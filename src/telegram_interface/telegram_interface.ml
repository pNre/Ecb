open Aws
open Async
open Async_unix
open Core

let handle_update update =
  match Telegram.parse_update update with
  | Command _ as command ->
    let payload = command |> Telegram.update_type_to_yojson |> Yojson.Safe.to_string in
    Print.printf "Pushing command: %s\n" payload;
    let queue = Sys.getenv_exn "COMMANDS_QUEUE_URL" in
    Sqs.post queue payload
    |> Deferred.Result.map ~f:(fun _ -> Some "{}")
    |> Deferred.Result.map_error ~f:(fun err ->
           let error_message = Http.string_of_error err in
           Lambda.make_error ~error_type:"sqs-send-event" ~error_message ())
  | _ -> Deferred.Result.fail (Lambda.make_error ~error_type:"unhandled-command" ())
;;

let perform (event : Lambda.api_gateway_proxy_event) =
  match event.Lambda.body with
  | Some body ->
    Print.printf "Telegram update:\n%s" body;
    let json = Yojson.Safe.from_string body in
    (match Telegram.update_of_yojson json with
    | Ok update -> handle_update update
    | Error error_message ->
      Deferred.Result.fail
        (Lambda.make_error ~error_type:"unhandled-update" ~error_message ()))
  | None ->
    Print.printf "Unhandled event\n";
    Deferred.Result.fail
      (Lambda.make_error ~error_type:"invalid-event" ~error_message:"body empty" ())
;;

let handler _context event =
  match Lambda.api_gateway_proxy_event_of_yojson event with
  | Ok event -> perform event
  | Error error_message ->
    Deferred.Result.fail
      (Lambda.make_error ~error_type:"unhandled-event" ~error_message ())
;;

let () =
  Deferred.forever () (fun () -> Lambda.next_invocation handler);
  never_returns (Scheduler.go ())
;;
