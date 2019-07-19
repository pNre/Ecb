open Aws
open Async
open Async_unix
open Core
open Telegram

let push_updates update =
  let events = Calendar_event.events_in_range update in
  Deferred.both (Subscription.subscribers ()) events
  >>= function
  | [], _ | _, [] ->
    Print.printf "No updates/subscribers\n";
    Deferred.Result.return None
  | subscribers, events ->
    let text =
      events
      |> List.map ~f:(fun (_, event) -> Calendar_event.description_of_event event)
      |> String.concat ~sep:"\n\n"
    in
    Print.printf "Sending events %s\n" text;
    let chat_ids = subscribers |> List.map ~f:Int64.of_string in
    Telegram.broadcast_message ~chat_ids ~text ()
    >>= (fun _ ->
          events
          |> List.map ~f:(fun event ->
                 Calendar_event.save_sent_notifications event update)
          |> Deferred.all)
    >>| fun _ -> Result.return None
;;

let update_events () =
  Events_source.events ()
  >>=? (fun events ->
         events
         |> List.map ~f:Calendar_event.save_event
         |> Deferred.Result.all
         |> Deferred.Result.map_error ~f:(fun err ->
                "request-error", Http.string_of_error err))
  >>= function
  | Ok _ -> Deferred.Result.return None
  | Error (error_type, error_message) ->
    Deferred.Result.fail (Lambda.make_error ~error_type ~error_message ())
;;

let dispatch_command request =
  match request with
  | Command ("/subscribe", _, chat_id) ->
    Subscription.add ~chat_id
    >>= (function
    | Ok _ -> handle_success chat_id "✌️"
    | Error _ -> handle_failure chat_id "Couldn't add subscription")
  | _ -> Deferred.Result.return ()
;;

let perform = function
  | Some (`Sqs_message Lambda.{ body = Some body; _ }) ->
    body
    |> Yojson.Safe.from_string
    |> Telegram.update_type_of_yojson
    |> Deferred.return
    |> Deferred.Result.bind ~f:dispatch_command
    >>| fun _ -> Ok None
  | Some (`Scheduled Lambda.{ event = "update-events" }) -> update_events ()
  | Some (`Scheduled Lambda.{ event = "push-updates" }) ->
    [ push_updates Week
    ; push_updates Day
    ; push_updates Hour_before
    ; push_updates On_time
    ]
    |> Deferred.all
    >>| fun _ -> Result.return None
  | Some _ | None ->
    Deferred.Result.fail (Lambda.make_error ~error_type:"invalid-record" ())
;;

let decode json =
  Print.printf "%s" (Yojson.Safe.to_string json);
  let open Lambda in
  let map_sqs json =
    json |> sqs_message_event_of_yojson_opt |> Option.map ~f:(fun s -> `Sqs_message s)
  in
  let map_scheduled_event json =
    json |> scheduled_event_of_yojson_opt |> Option.map ~f:(fun s -> `Scheduled s)
  in
  [ map_sqs; map_scheduled_event ] |> List.find_map ~f:(fun f -> f json)
;;

let handler _context event = event |> decode |> perform

let () =
  Deferred.forever () (fun () -> Lambda.next_invocation handler);
  never_returns (Scheduler.go ())
;;
