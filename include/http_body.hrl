-type http_body() :: #{binary() => http_body_key_value()}.

-type http_body_key_value() :: binary() | http_body_file() | true.

-type http_body_file() :: #{
  filename     => binary(), 
  data         => binary(), 
  content_type => any(), 
  encoding     => binary()
}.

-type http_body_content_type() :: {binary(), binary(), http_body_content_type_params()}.

-type http_body_content_type_params() :: #{binary() => binary()}.