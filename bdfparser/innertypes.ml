open Sexplib0.Sexp_conv

type property_val = 
  [ `Int of int
  | `String of string
  ] [@@deriving sexp]

type char_property_val = 
  [ `Encoding of int
  | `SWidth of int * int
  | `DWidth of int * int
  | `SWidth1 of int * int
  | `DWidth1 of int * int
  | `VVector of int * int
  | `BBox of int * int * int * int
  | `Bitmap of int list
  | `CharName of string
  ] [@@deriving sexp]

type header =
  [ `Version of float
  | `FontName of string
  | `Size of int * int * int 
  | `BoundingBox of int * int * int * int
  | `Comment of string
  | `Chars of int
  | `MetricSet of int
  | `ContentVersion of int
  | `Properties of (string * property_val) list
  | `Char of char_property_val list
  | `Noop
  ] [@@deriving sexp]
