type gpx = {
  version: float; (* Must be 1.1 *)
  creator: string;
  metadata: metadata option;
  wpt: wpt list;
  rte: rte list;
  trk: trk list;
  extensions: extension option;
}

 and metadata = {
   name: string option;
   desc: string option;
   author: person option;
   copyright: copyright option;
   link: link list;
   time: date_time option;
   keywords: string option;
   bounds: bounds option;
   extensions: extension option;
 }

 and wpt = {
   lat: latitude;
   lon: longitude;
   time: date_time option;
   ele: float option;
   magvar: degrees option;
   geoidheight: float option;
   name: string option;
   cmt: string option;
   desc: string option;
   src: string option;
   link: link list;
   sym: string option;
   typ: string option;
   fix: fix option;
   sat: int option;
   vdop: float option;
   hdop: float option;
   pdop: float option;
   ageofdgpsdata: float option;
   dgpsid: dgps_station option;
   extensions: extension option;
 }

 and rte = {
   name: string option;
   cmt: string option;
   desc: string option;
   src: string option;
   link: link list;
   number: int option;
   typ: string option;
   extensions: extension option;
   rtept: wpt list;
 }

 and trk = {
   name: string option;
   cmt: string option;
   desc: string option;
   src: string option;
   link: link list;
   number: int option;
   typ: string option;
   extensions: extension option;
   trkseg: trkseg list;
 }

 and extension = Xml.xml

 and trkseg = {
   trkpt: wpt list;
   extensions: extension option;
 }

 and copyright = {
   author: string;
   year: int option;
   license: string option;
 }

 and link = {
   href: string;
   text: string option;
   typ: string option;
 }

 and email = {
   id: string;
   domain: string;
 }

 and person = {
   name: string option;
   email: email option;
   link: link option;
 }

 and bounds = {
   minlat: latitude;
   minlon: longitude;
   maxlat: latitude;
   maxlon: longitude;
 }

 and latitude = float (* -180.0 <= value <= 180.0 *)

 and longitude = float (* -180.0 <= value <= 180.0 *)

 and degrees = float (* 0.0 <= value <= 360.0 *)

 and fix = FIX_none | FIX_2d | FIX_3d | FIX_dgps | FIX_pps

 and dgps_station = int (* 0 <= value <= 1023 *)

 and date_time = {
   year: int;
   month: int;
   day: int;
   hour: int;
   minute: int;
   second: float;
   timezone: timezone option;
 }

 and timezone = TIMEZONE_Z
              | TIMEZONE_plus of int * int
              | TIMEZONE_minus of int * int

let attrib = Xml.attrib

let opt_apply f = function None -> None | Some x -> Some (f x)

let opt_child xml tag =
  try Some (List.find (fun x -> Xml.tag x = tag) (Xml.children xml))
  with Not_found -> None

let opt_pcdata xml tag =
  opt_child xml tag
  |> opt_apply (fun x -> Xml.children x |> List.hd |> Xml.pcdata)

let opt_string xml tag =
  opt_pcdata xml tag

let opt_float xml tag =
  opt_pcdata xml tag |> opt_apply float_of_string

let opt_int xml tag =
  opt_pcdata xml tag |> opt_apply int_of_string

let list xml tag =
  try List.find (fun x -> Xml.tag x = tag) (Xml.children xml)
      |> Xml.children
  with Not_found -> []

let time_of_string s : date_time =
  let num = "[0-9]" in
  let grp s = "\\(" ^ s ^ "\\)" in
  (* FIXME: does not support float for seconds, nor timezone *)
  let r = grp ("-?"^num^num^num^num^"*") (* grp 1: yyyy *)
          ^ "-" ^ grp (num ^ num)        (* grp 2: mm *)
          ^ "-" ^ grp (num ^ num)        (* grp 3: dd *)
          ^ "T" ^ grp (num ^ num)        (* grp 4: hh *)
          ^ ":" ^ grp (num ^ num)        (* grp 5: mm *)
          ^ ":" ^ grp (num ^ num)        (* grp 6: ss *)
          ^ ".*"
          |> Str.regexp in
  if Str.string_match r s 0
  then { year     = int_of_string (Str.matched_group 1 s);
         month    = int_of_string (Str.matched_group 2 s);
         day      = int_of_string (Str.matched_group 3 s);
         hour     = int_of_string (Str.matched_group 4 s);
         minute   = int_of_string (Str.matched_group 5 s);
         second   = float_of_string (Str.matched_group 6 s);
         timezone = None }
  else failwith "Invalid date format"

let latitude_of_string = float_of_string

let longitude_of_string = float_of_string

let degrees_of_string = float_of_string

let fix_of_string = function
  | "none" -> FIX_none
  | "2d"   -> FIX_2d
  | "3d"   -> FIX_3d
  | "dgps" -> FIX_dgps
  | "pps"  -> FIX_pps

let link xml = {
    href = attrib xml "href";
    text = opt_string xml "text";
    typ  = opt_string xml "type";
}

let wpt xml =
  {
    lat           = attrib xml "lat" |> latitude_of_string;
    lon           = attrib xml "lon" |> longitude_of_string;
    time          = opt_pcdata xml "time" |> opt_apply time_of_string;
    ele           = opt_float xml "ele";
    magvar        = opt_pcdata xml "magvar" |> opt_apply degrees_of_string;
    geoidheight   = opt_float xml "geoidheight";
    name          = opt_string xml "name";
    cmt           = opt_string xml "cmt";
    desc          = opt_string xml "desc";
    src           = opt_string xml "src";
    link          = List.map link (list xml "link");
    sym           = opt_string xml "sym";
    typ           = opt_string xml "type";
    fix           = opt_pcdata xml "fix" |> opt_apply fix_of_string;
    sat           = opt_int xml "sat";
    vdop          = opt_float xml "vdop";
    hdop          = opt_float xml "hdop";
    pdop          = opt_float xml "pdop";
    ageofdgpsdata = opt_float xml "ageofdgpsdata";
    dgpsid        = opt_int xml "dgpsid";
    extensions    = opt_child xml "extensions";
  }

let rte xml =
  {
    name       = opt_string xml "name";
    cmt        = opt_string xml "cmt";
    desc       = opt_string xml "desc";
    src        = opt_string xml "src";
    link       = List.map link (list xml "link");
    number     = opt_int xml "number";
    typ        = opt_string xml "type";
    extensions = opt_child xml "extensions";
    rtept      = List.map wpt (list xml "rtept");
  }

let trkseg xml = {
  trkpt      = List.map wpt (list xml "trkpt");
  extensions = opt_child xml "extensions";
}

let trk xml = {
  name       = opt_string xml "name";
  cmt        = opt_string xml "cmt";
  desc       = opt_string xml "desc";
  src        = opt_string xml "src";
  link       = List.map link (list xml "link");
  number     = opt_int xml "number";
  typ        = opt_string xml "type";
  extensions = opt_child xml "extensions";
  trkseg     = List.map trkseg (list xml "trkseg");
}

let email xml = {
  id     = attrib xml "id";
  domain = attrib xml "domain";
}

let person xml = {
   name  = opt_string xml "name";
   email = opt_child xml "email" |> opt_apply email;
   link  = opt_child xml "link" |> opt_apply link;
}

let copyright xml = {
  author  = attrib xml "author";
  year    = opt_int xml "year";
  license = opt_string xml "license";
}

let bounds xml = {
  minlat = attrib xml "minlat" |> latitude_of_string;
  minlon = attrib xml "minlon" |> longitude_of_string;
  maxlat = attrib xml "maxlat" |> latitude_of_string;
  maxlon = attrib xml "maxlon" |> longitude_of_string;
}

let metadata xml = {
  name       = opt_string xml "name";
  desc       = opt_string xml "desc";
  author     = opt_child xml "author" |> opt_apply person;
  copyright  = opt_child xml "copyright" |> opt_apply copyright;
  link       = List.map link (list xml "link");
  time       = opt_pcdata xml "time" |> opt_apply time_of_string;
  keywords   = opt_string xml "keywords";
  bounds     = opt_child xml "bounds" |> opt_apply bounds;
  extensions = opt_child xml "extensions";
}

let gpx xml = {
    version    = attrib xml "version" |> float_of_string;
    creator    = attrib xml "creator";
    metadata   = opt_child xml "metadata" |> opt_apply metadata;
    wpt        = List.map wpt (list xml "wpt");
    rte        = List.map rte (list xml "rte");
    trk        = List.map trk (list xml "trk");
    extensions = opt_child xml "extensions";
  }

let of_xml = gpx

let to_xml gpx =
  failwith "Not implemented, yet."
