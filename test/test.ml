let _ =
    let source = Xml.parse_in stdin in
    let gpx_output = source |> Gpx.of_xml in
    print_endline "\n*** SOURCE ***\n";
    print_endline (Xml.to_string_fmt source);
    print_endline "\n*** GPX LIBRARY OUTPUT ***\n";
    print_endline (Xml.to_string_fmt (Gpx.to_xml gpx_output))
