open OUnit2
open Bdfparser

let test_empty_parsing _ =
  let lexbuf = Lexing.from_string "" in
  let ast = Parser.prog Lexer.read lexbuf in
  match ast with
  | None -> assert_failure "Got nothing"
  | Some l -> assert_equal l []

let test_basic_parsing _ =
  let prose =
{|STARTFONT 2.1
ENDFONT|} in
  let lexbuf = Lexing.from_string prose in
  let ast = Parser.prog Lexer.read lexbuf in
  let expected = [(`Version 2.1) ; (`Noop)] in
  match ast with
  | None -> assert_failure "Got nothing"
  | Some l -> assert_equal l expected

let test_comment _ =
  let prose =
{|STARTFONT 2.1
COMMENT "hello world 1"
ENDFONT|} in
  let lexbuf = Lexing.from_string prose in
  let ast = Parser.prog Lexer.read lexbuf in
  let expected = [(`Version 2.1) ; (`Comment {|"hello world 1"|}) ; (`Noop)] in
  match ast with
  | None -> assert_failure "Got nothing"
  | Some l -> assert_equal l expected

let test_unquoted_comment _ =
  let prose =
{|STARTFONT 2.1
COMMENT hello world r matey
ENDFONT|} in
  let lexbuf = Lexing.from_string prose in
  let ast = Parser.prog Lexer.read lexbuf in
  let expected = [(`Version 2.1) ; (`Comment "hello world r matey") ; (`Noop)] in
  match ast with
  | None -> assert_failure "Got nothing"
  | Some l -> assert_equal l expected

let test_unquoted_comment_2 _ =
  let prose =
{|STARTFONT 2.1
COMMENT Created by Fondu from a mac NFNT/FONT resource
ENDFONT|} in
  let lexbuf = Lexing.from_string prose in
  let ast = Parser.prog Lexer.read lexbuf in
  let expected = [(`Version 2.1) ; (`Comment "Created by Fondu from a mac NFNT/FONT resource") ; (`Noop)] in
  match ast with
  | None -> assert_failure "Got nothing"
  | Some l -> assert_equal l expected

let test_properties_empty _ =
  let prose =
{|STARTFONT 2.1
STARTPROPERTIES 0
ENDPROPERTIES
ENDFONT|} in
  let lexbuf = Lexing.from_string prose in
  let ast = Parser.prog Lexer.read lexbuf in
  let expected = [(`Version 2.1) ; (`Properties []) ; (`Noop)] in
  match ast with
  | None -> assert_failure "Got nothing"
  | Some l -> assert_equal l expected

let test_properties _ =
  let prose =
{|STARTFONT 2.1
STARTPROPERTIES 2
FONT_THING 42
FONT_OTHER "life, the universe, and everything"
ENDPROPERTIES
ENDFONT|} in
  let lexbuf = Lexing.from_string prose in
  let ast = Parser.prog Lexer.read lexbuf in
  let expected = [
    (`Version 2.1) ;
    (`Properties [
      ("FONT_THING", (`Int 42)) ;
      ("FONT_OTHER", (`String {|"life, the universe, and everything"|}))
    ]) ;
    (`Noop)
  ] in
  match ast with
  | None -> assert_failure "Got nothing"
  | Some l -> assert_equal l expected

let test_font_name _ =
  let prose =
{|STARTFONT 2.1
FONT -gnu-unifont-medium-r-normal--16-160-75-75-c-80-iso10646-1
ENDFONT|} in
  let lexbuf = Lexing.from_string prose in
  let ast = Parser.prog Lexer.read lexbuf in
  let expected = [
    (`Version 2.1) ;
    (`FontName "-gnu-unifont-medium-r-normal--16-160-75-75-c-80-iso10646-1") ;
    (`Noop)
  ] in
  match ast with
  | None -> assert_failure "Got nothing"
  | Some l -> assert_equal l expected

let test_font_size _ =
  let prose =
{|STARTFONT 2.1
SIZE 1 2 3
ENDFONT|} in
  let lexbuf = Lexing.from_string prose in
  let ast = Parser.prog Lexer.read lexbuf in
  let expected = [
    (`Version 2.1) ;
    (`Size (1, 2, 3)) ;
    (`Noop)
  ] in
  match ast with
  | None -> assert_failure "Got nothing"
  | Some l -> assert_equal l expected

let test_bounding_box _ =
  let prose =
{|STARTFONT 2.1
FONTBOUNDINGBOX 1 2 3 4
ENDFONT|} in
    let lexbuf = Lexing.from_string prose in
    let ast = Parser.prog Lexer.read lexbuf in
    let expected = [
      (`Version 2.1) ;
      (`BoundingBox (1, 2, 3, 4)) ;
      (`Noop)
    ] in
    match ast with
    | None -> assert_failure "Got nothing"
    | Some l -> assert_equal l expected

let test_content_version _ =
  let prose =
{|STARTFONT 2.1
CONTENTVERSION 42
ENDFONT|} in
    let lexbuf = Lexing.from_string prose in
    let ast = Parser.prog Lexer.read lexbuf in
    let expected = [
      (`Version 2.1) ;
      (`ContentVersion 42) ;
      (`Noop)
    ] in
    match ast with
    | None -> assert_failure "Got nothing"
    | Some l -> assert_equal l expected

let test_metric_set _ =
  let prose =
{|STARTFONT 2.1
METRICSET 1
ENDFONT|} in
    let lexbuf = Lexing.from_string prose in
    let ast = Parser.prog Lexer.read lexbuf in
    let expected = [
      (`Version 2.1) ;
      (`MetricSet 1) ;
      (`Noop)
    ] in
    match ast with
    | None -> assert_failure "Got nothing"
    | Some l -> assert_equal l expected

let test_no_chars _ =
  let prose =
{|STARTFONT 2.1
CHARS 0
ENDFONT|} in
    let lexbuf = Lexing.from_string prose in
    let ast = Parser.prog Lexer.read lexbuf in
    let expected = [
      (`Version 2.1) ;
      (`Chars 0) ;
      (`Noop)
    ] in
    match ast with
    | None -> assert_failure "Got nothing"
    | Some l -> assert_equal l expected

let test_empty_char _ =
  let prose =
{|STARTFONT 2.1
CHARS 1
STARTCHAR char0000
ENDCHAR
ENDFONT|} in
    let lexbuf = Lexing.from_string prose in
    let ast = Parser.prog Lexer.read lexbuf in
    let expected = [
      (`Version 2.1) ;
      (`Chars 1) ;
      (`Char [
        (`CharName "char0000")
      ]) ;
      (`Noop)
    ] in
    match ast with
    | None -> assert_failure "Got nothing"
    | Some l -> assert_equal l expected

let test_char_encoding _ =
  let prose =
{|STARTFONT 2.1
CHARS 1
STARTCHAR char0000
ENCODING 42
ENDCHAR
ENDFONT|} in
    let lexbuf = Lexing.from_string prose in
    let ast = Parser.prog Lexer.read lexbuf in
    let expected = [
      (`Version 2.1) ;
      (`Chars 1) ;
      (`Char [
        (`CharName "char0000") ;
        (`Encoding 42)
      ]) ;
      (`Noop)
    ] in
    match ast with
    | None -> assert_failure "Got nothing"
    | Some l -> assert_equal l expected

let test_char_bbx _ =
  let prose =
{|STARTFONT 2.1
CHARS 1
STARTCHAR char0000
BBX 1 2 3 4
ENDCHAR
ENDFONT|} in
    let lexbuf = Lexing.from_string prose in
    let ast = Parser.prog Lexer.read lexbuf in
    let expected = [
      (`Version 2.1) ;
      (`Chars 1) ;
      (`Char [
        (`CharName "char0000") ;
        (`BBox (1, 2, 3, 4))
      ]) ;
      (`Noop)
    ] in
    match ast with
    | None -> assert_failure "Got nothing"
    | Some l -> assert_equal l expected

let test_char_widths _ =
  let prose =
{|STARTFONT 2.1
CHARS 1
STARTCHAR char0000
SWIDTH 1 2
DWIDTH 3 4
ENDCHAR
ENDFONT|} in
    let lexbuf = Lexing.from_string prose in
    let ast = Parser.prog Lexer.read lexbuf in
    let expected = [
      (`Version 2.1) ;
      (`Chars 1) ;
      (`Char [
        (`CharName "char0000") ;
        (`SWidth (1, 2)) ;
        (`DWidth (3, 4))
      ]) ;
      (`Noop)
    ] in
    match ast with
    | None -> assert_failure "Got nothing"
    | Some l -> assert_equal l expected

let test_char_vvector _ =
  let prose =
{|STARTFONT 2.1
CHARS 1
STARTCHAR char0000
VVECTOR 1 2
ENDCHAR
ENDFONT|} in
    let lexbuf = Lexing.from_string prose in
    let ast = Parser.prog Lexer.read lexbuf in
    let expected = [
      (`Version 2.1) ;
      (`Chars 1) ;
      (`Char [
        (`CharName "char0000") ;
        (`VVector (1, 2))
      ]) ;
      (`Noop)
    ] in
    match ast with
    | None -> assert_failure "Got nothing"
    | Some l -> assert_equal l expected

let test_char_bitmap _ =
  let prose =
{|STARTFONT 2.1
CHARS 1
STARTCHAR char0000
BITMAP
00
01
0A
ENDCHAR
ENDFONT|} in
    let lexbuf = Lexing.from_string prose in
    let ast = Parser.prog Lexer.read lexbuf in
    let expected = [
      (`Version 2.1) ;
      (`Chars 1) ;
      (`Char [
        (`CharName "char0000") ;
        (`Bitmap [0 ; 1 ; 10])
      ]) ;
      (`Noop)
    ] in
    match ast with
    | None -> assert_failure "Got nothing"
    | Some l -> assert_equal l expected

let suite =
  "BasicParsingTests" >::: [
    "test_empty_parsing" >:: test_empty_parsing ;
    "test_basic_parsing" >:: test_basic_parsing ;
    "Comment" >:: test_comment ;
    "Unquoted comment" >:: test_unquoted_comment ;
    "Unquoted comment 2" >:: test_unquoted_comment_2 ;
    "Empty properties" >:: test_properties_empty ;
    "Filled properties" >:: test_properties ;
    "Font name" >:: test_font_name ;
    "Font size" >:: test_font_size ;
    "Bounding box" >:: test_bounding_box ;
    "Content version" >:: test_content_version ;
    "No chars" >:: test_no_chars ;
    "Metric set" >:: test_metric_set ;
    "Empty char definition" >:: test_empty_char ;
    "Char encoding" >:: test_char_encoding ;
    "Char bounding box" >:: test_char_bbx ;
    "Test S/Dwidth" >:: test_char_widths ;
    "Test VVector" >:: test_char_vvector ;
    "Test character bitmap" >:: test_char_bitmap ;
  ]

let () =
  run_test_tt_main suite
