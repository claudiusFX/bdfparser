(** A library that reads {{:https://en.wikipedia.org/wiki/Glyph_Bitmap_Distribution_Format}BDF bitmap font files} and will return per glyph bitmaps. *)

type t

module Glyph : sig
    type t

    val name: t -> string
    (** [name glyph] Returns the name in the font file of the specified glyph. *)

    val bbox: t -> (int * int * int * int)
    (** [bbox glyph] The underlying font bbox parameter. *)

    val dwidth: t -> (int * int)
    (** [dwidth glyph] The underlying font dwidth parameter. *)

    val dimensions: t -> (int * int * int * int)
    (** [dimensions glyph] Returns the width and height of the specified glyph, along with the x and y offsets from the drawing location to allow for descenders etc. *)

    val encoding: t -> int
    (** [encoding glyph] returns the character represented by the glyph *)

    val bitmap: t -> bytes
    (** [bitmap glyph] Renders a glyph to a series of bytes. The data is 1 bit per pixel,
        as a series of bytes per row, padded to the appropriate next byte boundary. *)
end

val create: string -> (t, string) result
(** [create filename] Instantiates a BDF font from the specified font file, or returns an error as to why loading failed.*)

val name: t -> string
(** [name font] Returns the X11 name of the font. *)

val bdf_version: t -> float
(** [bdf_version font] Returns the version number of the BDF spec used for the font. *)

val version: t -> int
(** [verson font] Returns the content version specified in the font, or 0 if not specified. *)

val glyph_count: t -> int
(** [glyph_count font] Returns a count of how many glyphs are in the font. *)

val glyph_of_char: t -> Uchar.t -> Glyph.t option
(** [glyph_of_char font char] Gets the glyph that maps to a given character in the font,
    or None if that character doesn't have an entry. *)
