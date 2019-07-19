let post queue message =
  let params = [ "Action", [ "SendMessage" ]; "MessageBody", [ message ] ] in
  let uri = Uri.of_string queue in
  let http_headers =
    Signature.sign_request
      ~service:"sqs"
      ~meth:"POST"
      ~uri
      ~body:(Uri.encoded_of_query params)
      ()
  in
  Http.post_form uri ~http_headers ~params ()
;;
