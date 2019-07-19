open Async
open Core
open Cohttp

type error =
  { error_type : string [@key "errorType"]
  ; error_message : string [@key "errorMessage"] [@default ""]
  ; stack_trace : string [@key "stackTrace"] [@default Printexc.get_backtrace ()]
  }
[@@deriving to_yojson { strict = false }, make]

type context =
  { aws_request_id : string
  ; invoked_function_arn : string option [@default None]
  ; deadline_ms : string option [@default None]
  ; trace_id : string option [@default None]
  ; client_context : string option [@default None]
  ; cognito_identity : string option [@default None]
  }
[@@deriving make]

type request_context = { account_id : string [@key "accountId"] }
[@@deriving of_yojson { strict = false }]

type api_gateway_proxy_event =
  { resource : string
  ; path : string
  ; http_method : string [@key "httpMethod"]
  ; headers : Yojson.Safe.t option [@default None]
  ; multi_value_headers : Yojson.Safe.t option [@default None] [@key "multiValueHeaders"]
  ; query_string_parameters : Yojson.Safe.t option
        [@default None] [@key "queryStringParameters"]
  ; multi_value_query_string_parameters : Yojson.Safe.t option
        [@default None] [@key "multiValueQueryStringParameters"]
  ; path_parameters : Yojson.Safe.t option [@default None] [@key "pathParameters"]
  ; stage_variables : Yojson.Safe.t option [@default None] [@key "stageVariables"]
  ; request_context : request_context option [@key "requestContext"]
  ; body : string option [@default None]
  ; is_base64_encoded : bool [@default false] [@key "isBase64Encoded"]
  }
[@@deriving of_yojson { strict = false }]

type sqs_message_event =
  { message_id : string [@key "messageId"]
  ; body : string option [@default None]
  }
[@@deriving of_yojson { strict = false }]

type scheduled_event = { event : string } [@@deriving of_yojson { strict = false }]

type records = { records : Yojson.Safe.t list [@key "Records"] }
[@@deriving of_yojson { strict = false }]

let runtime_path = "/2018-06-01/runtime/invocation/"

let context_of_response { Response.headers; _ } =
  let aws_request_id =
    Option.value_exn (Header.get headers "lambda-runtime-aws-request-id")
  in
  let invoked_function_arn = Header.get headers "lambda-runtime-invoked-function-arn" in
  let deadline_ms = Header.get headers "lambda-runtime-deadline-ms" in
  let trace_id = Header.get headers "lambda-runtime-trace-id" in
  let client_context = Header.get headers "lambda-runtime-client-context" in
  let cognito_identity = Header.get headers "lambda-runtime-cognito-identity" in
  make_context
    ~aws_request_id
    ~invoked_function_arn
    ~deadline_ms
    ~trace_id
    ~client_context
    ~cognito_identity
    ()
;;

let sqs_message_event_of_yojson_opt event =
  event
  |> records_of_yojson
  |> Result.ok
  |> Option.value_map ~default:[] ~f:(fun r -> r.records)
  |> List.filter_map ~f:(fun record ->
         record |> sqs_message_event_of_yojson |> Result.ok)
  |> List.hd
;;

let api_gateway_proxy_event_of_yojson_opt event =
  event |> api_gateway_proxy_event_of_yojson |> Result.ok
;;

let scheduled_event_of_yojson_opt event = event |> scheduled_event_of_yojson |> Result.ok

module Env = struct
  let host_and_port =
    "AWS_LAMBDA_RUNTIME_API" |> Sys.getenv_exn |> Host_and_port.of_string
  ;;

  let log_group_name () = "AWS_LAMBDA_LOG_GROUP_NAME" |> Sys.getenv
  let log_stream_name () = "AWS_LAMBDA_LOG_STREAM_NAME" |> Sys.getenv
  let function_name () = "AWS_LAMBDA_FUNCTION_NAME" |> Sys.getenv
  let function_version () = "AWS_LAMBDA_FUNCTION_VERSION" |> Sys.getenv
  let function_memory_size () = "AWS_LAMBDA_FUNCTION_MEMORY_SIZE" |> Sys.getenv
  let region () = "AWS_REGION" |> Sys.getenv
end

let request_uri path =
  Uri.make
    ~scheme:"http"
    ~host:Env.host_and_port.host
    ~port:Env.host_and_port.port
    ~path
    ()
;;

let invocation_response ~context ~response =
  let uri = request_uri (runtime_path ^ context.aws_request_id ^ "/response") in
  Http.request `POST uri ~body:response ()
;;

let invocation_error ~context ~error =
  let headers =
    [ "Content-Type", "application/json"
    ; "Lambda-Runtime-Function-Error-Type", error.error_type
    ]
  in
  let _ = error.stack_trace in
  let uri = request_uri (runtime_path ^ context.aws_request_id ^ "/error") in
  let json = error |> error_to_yojson |> Yojson.Safe.to_string in
  Http.request `POST uri ~http_headers:headers ~body:(Some json) ()
;;

let init_error ~error =
  let headers =
    [ "Content-Type", "application/json"
    ; "Lambda-Runtime-Function-Error-Type", error.error_type
    ]
  in
  let uri = request_uri (runtime_path ^ "init/error") in
  let json = error |> error_to_yojson |> Yojson.Safe.to_string in
  Http.request `POST uri ~http_headers:headers ~body:(Some json) ()
;;

let dispatch_event context event handler =
  handler context event
  >>= function
  | Ok response -> invocation_response ~context ~response
  | Error error -> invocation_error ~context ~error
;;

let next_invocation handler =
  let map_invocation response body =
    let context = context_of_response response in
    body
    |> Yojson.Safe.from_string
    |> fun event -> Deferred.Result.return (context, event)
  in
  let uri = request_uri (runtime_path ^ "next") in
  Http.request `GET uri ()
  >>=? (fun (response, body) -> Http.string_of_body body >>= map_invocation response)
  >>= (function
        | Ok (context, event) -> dispatch_event context event handler
        | Error err ->
          let error_message = Http.string_of_error err in
          let error = make_error ~error_type:"mapping-error" ~error_message () in
          init_error ~error)
  >>| ignore
;;
