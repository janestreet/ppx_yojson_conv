module Yojson_conv_error = Yojson_conv_error
module Yojson_conv = Yojson_conv
module Yojsonable = Yojsonable_intf
module Yojson = Yojson

external ignore : _ -> unit = "%ignore"
external poly_equal : 'a -> 'a -> bool = "%equal"

let ( ! ) : 'a ref -> 'a = fun x -> !x
