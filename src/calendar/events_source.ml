open Async
open Core

let parse_calendar data =
  let open Soup in
  let map_row row =
    let id = row |> R.id |> String.chop_prefix_exn ~prefix:"eventRowId_" in
    let date = row |> R.attribute "data-event-datetime" in
    let country = row $ "span.ceFlags" |> R.attribute "title" in
    let sentiment = row $ "td.sentiment" |> children |> count in
    let name = row $ "td.event > a" |> texts |> String.concat ~sep:"" |> String.strip in
    let key = Calendar_event.make_key ~id ~date () in
    let event = Calendar_event.make_event ~name ~country ~date ~sentiment () in
    key, event
  in
  let soup = parse data in
  let event_rows = soup $$ "tr.js-event-item" in
  event_rows |> to_list |> List.map ~f:map_row
;;

let events () =
  Print.printf "Fetching calendar\n";
  let uri =
    Uri.of_string
      "https://www.investing.com/economic-calendar/Service/getCalendarFilteredData"
  in
  let zone = Time.Zone.utc in
  let now = Time.now () in
  let date_from = Time.format now "%Y-%m-%d" ~zone in
  let date_to = Time.format (Time.add now (Time.Span.of_day 7.0)) "%Y-%m-%d" ~zone in
  let params =
    [ "country[]", [ "10" ]
    ; "country[]", [ "5" ]
    ; "country[]", [ "72" ]
    ; "importance[]", [ "3" ]
    ; "timeZone", [ "55" ]
    ; "timeFilter", [ "timeRemain" ]
    ; "limit_from", [ "0" ]
    ; "currentTab", [ "custom" ]
    ; "dateFrom", [ date_from ]
    ; "dateTo", [ date_to ]
    ]
  in
  Print.printf "Requesting events with: %s\n" (Uri.encoded_of_query params);
  let http_headers = [ "X-Requested-With", "XMLHttpRequest" ] in
  let request =
    Http.post_form uri ~http_headers ~params ()
    >>| Result.map_error ~f:(fun err -> "request-error", Http.string_of_error err)
  in
  request
  >>=? (fun (_, body) -> Http.string_of_body body >>| Result.return)
  >>|? fun body ->
  Print.printf "%s\n" body;
  body
  |> Yojson.Safe.from_string
  |> Yojson.Safe.Util.member "data"
  |> Yojson.Safe.Util.to_string
  |> parse_calendar
;;
