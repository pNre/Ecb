open Core

let region = "AWS_REGION" |> Sys.getenv_exn
let access_key_id = "AWS_ACCESS_KEY_ID" |> Sys.getenv_exn
let secret_access_key = "AWS_SECRET_ACCESS_KEY" |> Sys.getenv_exn
let session_token = "AWS_SESSION_TOKEN" |> Sys.getenv_exn
