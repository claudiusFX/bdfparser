
module Uchar_map = Map.Make(Uchar)

module Glyph = struct
  type t = {
    name : string;
    encoding : int;
    swidth : int * int;
    swidth1 : int * int;
    dwidth : int * int;
    dwidth1 : int * int ;
    vvector : int * int ;
    bounding_box : int * int * int * int ;
    bitmap : bytes;
  }

  let name g  =
    g.name

  let bbox g =
    g.bounding_box

  let dwidth g =
    g.dwidth

  let encoding g =
    g.encoding

  let dimensions g =
    (* This is totes wrong, but enough to bootstrap with I think for basic western chars *)
    let x, _ = g.dwidth in
    let _, y, ox, oy = g.bounding_box in
    (x, y, ox, oy)

  let bitmap g =
    g.bitmap

  let innerchar_to_glyph (ic : Innertypes.char_property_val list) : t =
    let default_glyph = {
      name = "" ;
      encoding = 0 ;
      swidth = (0, 0) ;
      swidth1 = (0, 0) ;
      dwidth = (0, 0) ;
      dwidth1 = (0, 0) ;
      vvector = (0, 0) ;
      bounding_box = (0, 0, 0, 0) ;
      bitmap = Bytes.empty ;
    } in
    List.fold_left (fun (acc : t) (item : Innertypes.char_property_val) : t ->
      match item with
      | `CharName n -> { acc with name=n }
      | `Encoding e -> { acc with encoding=e }
      | `SWidth s -> { acc with swidth=s }
      | `DWidth s -> { acc with dwidth=s }
      | `SWidth1 s -> { acc with swidth1=s }
      | `DWidth1 s -> { acc with dwidth1=s }
      | `VVector v -> { acc with vvector=v }
      | `BBox b -> { acc with bounding_box=b }
      | `Bitmap d -> (
        (* this is poor, in that I currently assume the bbox has come first. In the spec the definition of
           the bitmap field depends on the values in bbox, but it strictly doesn't say bbox must come first
           so at some point we should do better, but for bootstrapping purposes I'm going to assume bbox
           is valid by this point. *)
        let w, _, _, _ = acc.bounding_box in
        (* TODO we should asssert length of bitmap is same as h in bbox as per spec *)
        let bytes_per_line = Int.of_float (ceil ((Float.of_int w) /. 8.)) in
        let h = List.length d in
        let bitmap = Bytes.create (h * bytes_per_line) in
        List.iteri (fun r v ->
          for c = 0 to (bytes_per_line - 1) do
            Bytes.set bitmap ((r * bytes_per_line) + c) (char_of_int ((Int.shift_right v (8 * c)) land 0xFF))
          done
        ) d;
        { acc with bitmap = bitmap }
      )
    ) default_glyph ic
end

type t = {
  version : float ;
  name : string ;
  size : int * int * int ;
  bounding_box : int * int * int * int ;
  content_version : int ;
  metric_set : int ;
  properties : (string * Innertypes.property_val) list;
  glyphs : Glyph.t array;
  map: int Uchar_map.t;
}

let default_t = {
  version = 0. ;
  name = "" ;
  size = (0, 0, 0) ;
  bounding_box = (0, 0, 0, 0) ;
  content_version = 0 ;
  metric_set = 0 ;
  properties = [] ;
  glyphs = [||] ;
  map = Uchar_map.empty;
}




let create (filename : string) : (t, string) result =
  let ast = In_channel.with_open_text filename (fun ic ->
    let lexbuf = Lexing.from_channel ic in
    Parser.prog Lexer.read lexbuf
  ) in
  match ast with
  | None -> (Error "No file")
  | Some ast -> (
    Ok (
      let fnt = List.fold_left (fun (acc : t) (item : Innertypes.header) : t ->
        match item with
        | `Version v -> { acc with version=v }
        | `Size s -> { acc with size=s }
        | `FontName n -> { acc with name=n }
        | `BoundingBox b -> { acc with bounding_box=b }
        | `Comment _ -> acc
        | `Chars _ -> acc
        | `MetricSet s -> { acc with metric_set=s }
        | `ContentVersion c -> { acc with content_version=c }
        | `Properties p -> { acc with properties=(List.concat [acc.properties ; p]) }
        | `Char c -> (
          let g = Glyph.innerchar_to_glyph c in
          { acc with glyphs=(Array.of_list (g :: (Array.to_list acc.glyphs)))}
        )
        | `Noop -> acc
        ) default_t ast
      in
      (* having built it, now work out the lookup map *)
      let _, map =
        Array.fold_left
          (fun (i, acc ) g ->
             let u = Uchar.of_int (Glyph.encoding g) in
             (i + 1, Uchar_map.add u i acc)
          ) (0, Uchar_map.empty) fnt.glyphs
      in
      { fnt with map }
    )
  )

let name t =
  t.name

let bdf_version t =
  t.version

let version t =
  t.content_version

let glyph_count t =
  Array.length t.glyphs

let glyph_of_char (font : t) (u : Uchar.t) : Glyph.t option =
  match (Uchar_map.find_opt u font.map) with
  | None -> None
  | Some index -> (
    match ((index >= 0) && (index < Array.length font.glyphs)) with
    | false -> None
    | true -> Some font.glyphs.(index)
  )
