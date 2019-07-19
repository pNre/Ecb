open Async
open Core

module Attribute_value = struct
  module S = struct
    type t = { value : string [@key "S"] [@main] }
    [@@deriving yojson { strict = false }, show]

    let to_string s = s.value
    let of_string s = { value = s }
    let yojson_of_string = Fn.compose to_yojson of_string
  end

  module SS = struct
    type t = { values : string list [@key "SS"] [@main] }
    [@@deriving yojson { strict = false }, show]

    let insert_string ss ~s = { values = s :: ss.values }
    let of_string_list ss = { values = ss }
    let yojson_of_string_list = Fn.compose to_yojson of_string_list
  end

  module N = struct
    type t = { value : string [@key "N"] [@main] }
    [@@deriving yojson { strict = false }, show]

    let to_int s = int_of_string s.value
    let of_string s = { value = s }
    let of_int i = of_string (string_of_int i)
    let yojson_of_string = Fn.compose to_yojson of_string
  end

  let expression_values_of_yojson json =
    let open Yojson.Safe.Util in
    json
    |> to_assoc
    |> List.map ~f:(fun (key, value) -> ":" ^ key, value)
    |> fun assoc -> `Assoc assoc
  ;;
end

type update_item_request =
  { key : Yojson.Safe.t [@key "Key"]
  ; update_expression : string option [@key "UpdateExpression"] [@default None]
  ; expression_attribute_values : Yojson.Safe.t option
        [@key "ExpressionAttributeValues"] [@default None]
  ; table_name : string [@key "TableName"]
  }
[@@deriving to_yojson { strict = false }, make, show]

type query_request =
  { table_name : string [@key "TableName"]
  ; consistent_read : bool option [@key "ConsistentRead"] [@default None]
  ; filter_expression : string option [@key "FilterExpression"] [@default None]
  ; index_name : string option [@key "IndexName"] [@default None]
  ; key_condition_expression : string option
        [@key "KeyConditionExpression"] [@default None]
  ; expression_attribute_values : Yojson.Safe.t option
        [@key "ExpressionAttributeValues"] [@default None]
  ; limit : int option [@key "Limit"] [@default None]
  }
[@@deriving to_yojson { strict = false }, make]

type query_result =
  { count : int [@key "Count"]
  ; items : Yojson.Safe.t list [@key "Items"]
  }
[@@deriving of_yojson { strict = false }, make, show]

let headers action =
  [ "Content-Type", "application/json"; "X-Amz-Target", "DynamoDB_20120810." ^ action ]
;;

let uri () =
  Uri.make
    ~scheme:"https"
    ~host:("dynamodb." ^ Env.region ^ ".amazonaws.com")
    ~path:"/"
    ()
;;

let update_item request =
  let request = request |> update_item_request_to_yojson |> Yojson.Safe.to_string in
  let uri = uri () in
  let http_headers =
    Signature.sign_request
      ~service:"dynamodb"
      ~meth:"POST"
      ~uri
      ~headers:(headers "UpdateItem")
      ~body:request
      ()
  in
  Http.request `POST uri ~http_headers ~body:(Some request) ()
;;

let query request =
  let request = request |> query_request_to_yojson |> Yojson.Safe.to_string in
  let uri = uri () in
  let http_headers =
    Signature.sign_request
      ~service:"dynamodb"
      ~meth:"POST"
      ~uri
      ~headers:(headers "Query")
      ~body:request
      ()
  in
  Http.request `POST uri ~http_headers ~body:(Some request) ()
  >>=? fun (request, body) ->
  body
  |> Http.string_of_body
  >>| Yojson.Safe.from_string
  >>| query_result_of_yojson
  >>| Result.map_error ~f:(fun _ -> Http.Response (uri, request, body))
;;
