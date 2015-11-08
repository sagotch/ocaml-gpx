let walk directory pattern =
  let select str = Re_str.string_match (Re_str.regexp pattern) str 0 in
  let rec aux acc = function
    | [] -> acc
    | dir :: rest ->
      let contents = Array.to_list (Sys.readdir dir) in
      let contents = List.rev_map (Filename.concat dir) contents in
      let dirs, files =
        List.fold_left (fun (dirs, files) kind ->
          match (Unix.stat kind).Unix.st_kind with
          | Unix.S_REG -> (dirs, kind :: files)
          | Unix.S_DIR -> (kind :: dirs, files)
          | _ -> (dirs, files))
          ([], []) contents
      in
      let matched = List.filter select files in
      aux (matched @ acc) (dirs @ rest)
  in
  aux [] [directory]

let gpxs directory = walk directory ".*\\.gpx"

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  s |> Bytes.to_string

let gpx =
  let module M =
    struct
      type t = Gpx.gpx

      let pp fmt x = Format.pp_print_string fmt (Gpx.to_xml x |> Xml.to_string)

      let equal = (=)
    end
  in (module M : Alcotest.TESTABLE with type t = M.t)

let make_test filename =
  Printf.sprintf "%s" (Filename.basename filename |> Filename.chop_extension),
  `Quick,
  (fun () ->
    let data = load_file filename
      |> Xml.parse_string
      |> Gpx.of_xml
    in
    Alcotest.(check gpx) "same" data data)

let tests =
  gpxs "./src_test/"
  |> List.map make_test

let () =
  Alcotest.run "GPX test" ["set", tests]
