open Bdfparser
open Sexplib0.Sexp_conv
open Expect_test_helpers_core

let lex_print string =
  let lexbuf = Lexing.from_string string in
  let ast = Parser.prog Lexer.read lexbuf in
  print_s ([%sexp_of: Innertypes.header list option] ast)

let%expect_test "empty_parsing" =
  lex_print "";
  [%expect {| (()) |}]

let%expect_test "test_basic_parsing" =
  lex_print
{|STARTFONT 2.1
ENDFONT|};
  [%expect {| (((Version 2.1) Noop)) |}]

let%expect_test "test_comment" =
  lex_print
{|STARTFONT 2.1
COMMENT "hello world 1"
ENDFONT|};
  [%expect {|
    ((
      (Version 2.1)
      (Comment "\"hello world 1\"")
      Noop))
    |}]

let%expect_test "test_unquoted_comment" =
  lex_print
{|STARTFONT 2.1
COMMENT hello world r matey
ENDFONT|};
  [%expect {|
    ((
      (Version 2.1)
      (Comment "hello world r matey")
      Noop))
    |}]

let%expect_test "test_unquoted_comment_2" =
  lex_print
{|STARTFONT 2.1
COMMENT Created by Fondu from a mac NFNT/FONT resource
ENDFONT|};
  [%expect {|
    ((
      (Version 2.1)
      (Comment "Created by Fondu from a mac NFNT/FONT resource")
      Noop))
    |}]

let%expect_test "test_properties_empty" =
  lex_print
{|STARTFONT 2.1
STARTPROPERTIES 0
ENDPROPERTIES
ENDFONT|};
  [%expect {| (((Version 2.1) (Properties ()) Noop)) |}]

let%expect_test "test_properties" =
  lex_print
{|STARTFONT 2.1
STARTPROPERTIES 2
FONT_THING 42
FONT_OTHER "life, the universe, and everything"
ENDPROPERTIES
ENDFONT|};
  [%expect {|
    ((
      (Version 2.1)
      (Properties (
        (FONT_THING (Int 42))
        (FONT_OTHER (String "\"life, the universe, and everything\""))))
      Noop))
    |}]

let%expect_test "test_font_name" =
  lex_print
{|STARTFONT 2.1
FONT -gnu-unifont-medium-r-normal--16-160-75-75-c-80-iso10646-1
ENDFONT|};
  [%expect {|
    ((
      (Version 2.1)
      (FontName -gnu-unifont-medium-r-normal--16-160-75-75-c-80-iso10646-1)
      Noop))
    |}]

let%expect_test "test_font_size" =
  lex_print
{|STARTFONT 2.1
SIZE 1 2 3
ENDFONT|};
  [%expect {| (((Version 2.1) (Size (1 2 3)) Noop)) |}]

let%expect_test "test_bounding_box" =
  lex_print
{|STARTFONT 2.1
FONTBOUNDINGBOX 1 2 3 4
ENDFONT|};
  [%expect {| (((Version 2.1) (BoundingBox (1 2 3 4)) Noop)) |}]

let%expect_test "test_content_version" =
  lex_print
{|STARTFONT 2.1
CONTENTVERSION 42
ENDFONT|};
  [%expect {|
    ((
      (Version        2.1)
      (ContentVersion 42)
      Noop))
    |}]

let%expect_test "test_metric_set" =
  lex_print
{|STARTFONT 2.1
METRICSET 1
ENDFONT|};
  [%expect {|
    ((
      (Version   2.1)
      (MetricSet 1)
      Noop))
    |}]

let%expect_test "test_no_chars" =
  lex_print
{|STARTFONT 2.1
CHARS 0
ENDFONT|};
  [%expect {|
    ((
      (Version 2.1)
      (Chars   0)
      Noop))
    |}]

let%expect_test "test_empty_char" =
  lex_print
{|STARTFONT 2.1
CHARS 1
STARTCHAR char0000
ENDCHAR
ENDFONT|};
  [%expect {|
    ((
      (Version 2.1)
      (Chars   1)
      (Char ((CharName char0000)))
      Noop))
    |}]

let%expect_test "test_char_encoding" =
  lex_print
{|STARTFONT 2.1
CHARS 1
STARTCHAR char0000
ENCODING 42
ENDCHAR
ENDFONT|};
  [%expect {|
    ((
      (Version 2.1)
      (Chars   1)
      (Char (
        (CharName char0000)
        (Encoding 42)))
      Noop))
    |}]

let%expect_test "test_char_bbx" =
  lex_print
{|STARTFONT 2.1
CHARS 1
STARTCHAR char0000
BBX 1 2 3 4
ENDCHAR
ENDFONT|};
  [%expect {|
    ((
      (Version 2.1)
      (Chars   1)
      (Char ((CharName char0000) (BBox (1 2 3 4))))
      Noop))
    |}]

let%expect_test "test_char_widths" =
  lex_print
{|STARTFONT 2.1
CHARS 1
STARTCHAR char0000
SWIDTH 1 2
DWIDTH 3 4
ENDCHAR
ENDFONT|};
  [%expect {|
    ((
      (Version 2.1)
      (Chars   1)
      (Char (
        (CharName char0000)
        (SWidth (1 2))
        (DWidth (3 4))))
      Noop))
    |}]

let%expect_test "test_char_vvector" =
  lex_print
{|STARTFONT 2.1
CHARS 1
STARTCHAR char0000
VVECTOR 1 2
ENDCHAR
ENDFONT|};
  [%expect {|
    ((
      (Version 2.1)
      (Chars   1)
      (Char ((CharName char0000) (VVector (1 2))))
      Noop))
    |}]

let%expect_test "test_char_bitmap" =
  lex_print
{|STARTFONT 2.1
CHARS 1
STARTCHAR char0000
BITMAP
00
01
0A
ENDCHAR
ENDFONT|};
  [%expect {|
    ((
      (Version 2.1)
      (Chars   1)
      (Char ((CharName char0000) (Bitmap (0 1 10))))
      Noop))
    |}]
