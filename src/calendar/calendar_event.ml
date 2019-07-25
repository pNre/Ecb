open Core
open Aws.Dynamodb
open Async
open Async_unix

let table_name = Sys.getenv_exn "EVENTS_TABLE"

type key =
  { entity_type : Attribute_value.S.t
  ; event_key : Attribute_value.S.t
  }
[@@deriving yojson { strict = false }]

type event =
  { event_day : Attribute_value.S.t
  ; event_time : Attribute_value.S.t
  ; event_name : Attribute_value.S.t
  ; event_country : Attribute_value.S.t
  ; event_sentiment : Attribute_value.N.t
  ; event_sent_notifications : Attribute_value.SS.t option [@default None]
  }
[@@deriving yojson { strict = false }]

type event_filter_expression_attributes =
  { entity_type : Attribute_value.S.t
  ; start_date : Attribute_value.S.t
  ; end_date : Attribute_value.S.t
  ; sent_notification : Attribute_value.S.t option [@default None]
  }
[@@deriving to_yojson { strict = false }, make]

type t = key * event

type update =
  | Week
  | Day
  | Hour_before
  | On_time

let string_of_update = function
  | Week -> "WEEK"
  | Day -> "DAY"
  | Hour_before -> "HOUR_BEFORE"
  | On_time -> "ON_TIME"
;;

type time_range =
  | Days of string * string
  | Day of string
  | Hours of string * string * string

let time_range_of_update update =
  let now = Time.now () in
  let zone = Time.Zone.utc in
  let today = Time.format now "%Y%m%d" ~zone in
  match update with
  | Week ->
    let week = Time.Span.of_day 7.0 in
    let next_week = Time.add now week in
    let next_week = Time.format next_week "%Y%m%d" ~zone in
    Days (today, next_week)
  | Day -> Day today
  | Hour_before ->
    let open Time in
    let now = Ofday.now ~zone in
    let time = Ofday.to_sec_string now in
    let next_hour =
      Ofday.add now Time.Span.hour
      |> Option.value ~default:Ofday.start_of_next_day
      |> Ofday.to_sec_string
    in
    Hours (today, time, next_hour)
  | On_time ->
    let open Time in
    let now = Ofday.now ~zone in
    let time = Ofday.to_sec_string now in
    let next_hour =
      Ofday.add now (Time.Span.of_min 10.0)
      |> Option.value ~default:Ofday.start_of_next_day
      |> Ofday.to_sec_string
    in
    Hours (today, time, next_hour)
;;

let request_of_update update =
  let update_type = string_of_update update in
  let time_range = time_range_of_update update in
  let key_conditions =
    match time_range with
    | Days _ -> "event_key BETWEEN :start_date AND :end_date"
    | Day _ | Hours _ -> "begins_with(event_key, :date)"
  in
  let key_condition_expression =
    [ "entity_type = :entity_type"; key_conditions ] |> String.concat ~sep:" AND "
  in
  let filter_conditions =
    match time_range with
    | Days _ | Day _ -> None
    | Hours _ -> Some "event_time BETWEEN :start_time AND :end_time"
  in
  let filter_expression =
    [ Some "NOT contains (event_sent_notifications, :sent_notification)"
    ; filter_conditions
    ]
    |> List.filter_map ~f:Fun.id
    |> String.concat ~sep:" AND "
  in
  let yojson_attribute_of_string =
    Fn.compose Attribute_value.S.to_yojson Attribute_value.S.of_string
  in
  let base_expression_attribute_values =
    `Assoc
      [ ":entity_type", yojson_attribute_of_string "EVENT"
      ; ":sent_notification", yojson_attribute_of_string update_type
      ]
  in
  let expression_attribute_values =
    `Assoc
      (match time_range with
      | Days (start_date, end_date) ->
        [ ":start_date", yojson_attribute_of_string start_date
        ; ":end_date", yojson_attribute_of_string end_date
        ]
      | Day date -> [ ":date", yojson_attribute_of_string date ]
      | Hours (date, start_time, end_time) ->
        [ ":date", yojson_attribute_of_string date
        ; ":start_time", yojson_attribute_of_string start_time
        ; ":end_time", yojson_attribute_of_string end_time
        ])
    |> Yojson.Safe.Util.combine base_expression_attribute_values
  in
  key_condition_expression, filter_expression, expression_attribute_values
;;

let flag_of_country country =
  match String.strip country with
  | "Euro Zone" -> " 🇪🇺"
  | "United States" -> " 🇺🇸"
  | "Italy" -> " 🇮🇹"
  | _ -> ""
;;

let description_of_event
    { event_name; event_sentiment; event_country; event_day; event_time; _ }
  =
  let sentiment =
    Attribute_value.N.to_int event_sentiment
    |> List.init ~f:(fun _ -> "🐂")
    |> String.concat ~sep:""
  in
  let name = event_name.value in
  let country = event_country.value in
  let flag = flag_of_country country in
  let day = event_day.value in
  let time = event_time.value in
  let zone = Time.Zone.utc in
  let time = Time.parse (day ^ " " ^ time) ~fmt:"%Y/%m/%d %T" ~zone in
  let zone = Time.Zone.find_exn "Europe/Rome" in
  let day = Time.format time "%Y/%m/%d" ~zone in
  let time = Time.format time "%T" ~zone in
  sprintf
    "*%s*\nBulliness: %s\nCountry: %s%s\nDay: %s\nTime: %s"
    name
    sentiment
    country
    flag
    day
    time
;;

let make_event ~name ~country ~date ~sentiment () =
  let open Attribute_value in
  let zone = Time.Zone.utc in
  let time = Time.parse date ~fmt:"%Y/%m/%d %T" ~zone in
  let event_date = Time.format time "%Y/%m/%d" ~zone in
  let event_time = time |> Time.to_ofday ~zone |> Time.Ofday.to_sec_string in
  { event_day = S.of_string event_date
  ; event_time = S.of_string event_time
  ; event_name = S.of_string name
  ; event_country = S.of_string country
  ; event_sentiment = N.of_int sentiment
  ; event_sent_notifications = None
  }
;;

let make_key ~id ~date () =
  let open Attribute_value in
  let zone = Time.Zone.utc in
  let time = Time.parse date ~fmt:"%Y/%m/%d %T" ~zone in
  let key = Time.format time "%Y%m%d" ~zone ^ ":" ^ id in
  { entity_type = S.of_string "EVENT"; event_key = S.of_string key }
;;

let of_yojson j =
  let key = key_of_yojson j in
  let event = event_of_yojson j in
  Result.combine
    key
    event
    ~ok:(fun key event -> key, event)
    ~err:(fun key_err event_err -> String.concat [ key_err; event_err ] ~sep:"\n")
;;

let to_yojson key event =
  let key = key_to_yojson key in
  let event = event_to_yojson event in
  Yojson.Safe.Util.combine key event
;;

let save_event (key, event) =
  let key = key_to_yojson key in
  let expression_attribute_values =
    event
    |> event_to_yojson
    |> Attribute_value.expression_values_of_yojson
    |> Option.return
  in
  let update_expression =
    Some
      "SET event_day = :event_day, event_time = :event_time, event_name = :event_name, \
       event_country = :event_country, event_sentiment = :event_sentiment"
  in
  let request =
    make_update_item_request
      ~key
      ~update_expression
      ~expression_attribute_values
      ~table_name
      ()
  in
  Print.printf
    "Request: %s"
    (request |> update_item_request_to_yojson |> Yojson.Safe.to_string);
  update_item request
;;

let save_sent_notifications (key, event) update =
  let key = key_to_yojson key in
  let event_update_string = string_of_update update in
  let pushed_updates =
    event.event_sent_notifications
    |> Option.map ~f:(Attribute_value.SS.insert_string ~s:event_update_string)
    |> Option.value ~default:(Attribute_value.SS.of_string_list [ event_update_string ])
  in
  let expression_attribute_values =
    Some (`Assoc [ ":sent_notifications", Attribute_value.SS.to_yojson pushed_updates ])
  in
  let update_expression = Some "SET event_sent_notifications = :sent_notifications" in
  let request =
    make_update_item_request
      ~key
      ~update_expression
      ~expression_attribute_values
      ~table_name
      ()
  in
  Print.printf
    "Save sent request: %s"
    (request |> update_item_request_to_yojson |> Yojson.Safe.to_string);
  update_item request
;;

let events_in_range update =
  let key_condition_expression, filter_expression, expression_attribute_values =
    request_of_update update
  in
  let request =
    make_query_request
      ~table_name
      ~key_condition_expression:(Some key_condition_expression)
      ~filter_expression:(Some filter_expression)
      ~expression_attribute_values:(Some expression_attribute_values)
      ()
  in
  Print.printf "%s\n" (request |> query_request_to_yojson |> Yojson.Safe.to_string);
  query request
  >>= function
  | Ok result ->
    let event_of_item item = item |> of_yojson |> Result.ok in
    result.items |> List.filter_map ~f:event_of_item |> Deferred.return
  | Error _ -> Deferred.return []
;;
