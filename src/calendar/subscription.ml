open Aws
open Async
open Dynamodb
open Core
open Core.Poly

let table_name = Sys.getenv_exn "SUBSCRIBERS_TABLE"

type subscription =
  { subscription : Attribute_value.S.t [@main]
  ; subscriber : Attribute_value.S.t option [@default None]
  }
[@@deriving yojson { strict = false }, make]

let subscribers_request ?(subscription = "ALL") () =
  let open Attribute_value in
  let subscription =
    subscription
    |> S.of_string
    |> make_subscription
    |> subscription_to_yojson
    |> expression_values_of_yojson
  in
  let key_condition_expression = Some "subscription = :subscription" in
  let expression_attribute_values = Some subscription in
  make_query_request
    ~table_name
    ~key_condition_expression
    ~expression_attribute_values
    ()
;;

let subscribers () =
  let open Attribute_value in
  let subscriber_of_item item =
    item
    |> subscription_of_yojson
    |> Result.map ~f:(fun s -> s.subscriber |> Option.map ~f:S.to_string)
    |> Result.ok
    |> Option.join
  in
  query (subscribers_request ())
  >>= function
  | Ok result -> result.items |> List.filter_map ~f:subscriber_of_item |> Deferred.return
  | Error _ -> Deferred.return []
;;

let add ~chat_id =
  let open Attribute_value in
  let subscription = "ALL" |> S.of_string in
  let subscriber = chat_id |> Int64.to_string |> S.of_string |> Option.return in
  let key = make_subscription ~subscriber subscription |> subscription_to_yojson in
  let request = make_update_item_request ~key ~table_name () in
  update_item request
;;
