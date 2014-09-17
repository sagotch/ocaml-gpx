let errors = ref []

let test file =
  let source = Xml.parse_file file in
  let result = source |> Gpx.of_xml |> Gpx.to_xml in
  if Xml.to_string_fmt source <> Xml.to_string_fmt result then
    errors := (file, source, result) :: !errors

let () =
  test "samples/route.gpx";
  test "samples/track.gpx";
  test "samples/waypoints.gpx";
  test "samples/fells_loop.gpx";
  if !errors = [] then print_endline "All tests ran fine!"
  else (
    List.iter
      (fun (file, source, result) ->
        print_endline @@ "\n*** Error while testing " ^ file ^ " ***";
        print_endline @@ "\n*** SOURCE (" ^ file ^ ") ***\n";
        print_endline (Xml.to_string_fmt source);
        print_endline @@ "\n*** GPX LIBRARY OUTPUT (" ^ file ^ ") ***\n";
        print_endline (Xml.to_string_fmt result))
      !errors;
    assert false)
