type gpx = {
  version: string; (* Must be 1.1 *)
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

 and date_time = (float * float option) (* UTC time, timezone offset *)

let opt_apply f = function None -> None | Some x -> Some (f x)

module Of_XML = struct

    let attrib = Xml.attrib

    let opt_child xml tag =
      try Some (List.find (fun x -> Xml.tag x = tag) (Xml.children xml))
      with Not_found -> None

    let opt_pcdata xml tag =
      opt_child xml tag
      |> opt_apply (fun x -> Xml.children x
			     |> List.map Xml.pcdata
			     |> String.concat "\n")

    let opt_string xml tag =
      opt_pcdata xml tag

    let opt_float xml tag =
      opt_pcdata xml tag |> opt_apply float_of_string

    let opt_int xml tag =
      opt_pcdata xml tag |> opt_apply int_of_string

    let list xml tag =
      try List.find_all (fun x -> Xml.tag x = tag) (Xml.children xml)
      with Not_found -> []

    let time_of_string s : date_time =
      ISO8601.Permissive.datetime_tz s

    let fix_of_string = function
      | "none" -> FIX_none
      | "2d"   -> FIX_2d
      | "3d"   -> FIX_3d
      | "dgps" -> FIX_dgps
      | "pps"  -> FIX_pps
      | _      -> raise (Invalid_argument "fix_of_string")

    let link xml = {
        href = attrib xml "href";
        text = opt_string xml "text";
        typ  = opt_string xml "type";
      }

    let wpt xml = {
        lat           = attrib xml "lat" |> float_of_string;
        lon           = attrib xml "lon" |> float_of_string;
        time          = opt_pcdata xml "time" |> opt_apply time_of_string;
        ele           = opt_float xml "ele";
        magvar        = opt_pcdata xml "magvar" |> opt_apply float_of_string;
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

    let rte xml = {
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
        minlat = attrib xml "minlat" |> float_of_string;
        minlon = attrib xml "minlon" |> float_of_string;
        maxlat = attrib xml "maxlat" |> float_of_string;
        maxlon = attrib xml "maxlon" |> float_of_string;
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
        version    = attrib xml "version";
        creator    = attrib xml "creator";
        metadata   = opt_child xml "metadata" |> opt_apply metadata;
        wpt        = List.map wpt (list xml "wpt");
        rte        = List.map rte (list xml "rte");
        trk        = List.map trk (list xml "trk");
        extensions = opt_child xml "extensions";
      }

    let trkpt = wpt
    let rtept = wpt
end

module To_XML = struct

    let (@@@) hd tl = match hd with None -> tl | Some hd -> hd :: tl

    let string_of_fix = function
      | FIX_none -> "none"
      | FIX_2d   -> "2d"
      | FIX_3d   -> "3d"
      | FIX_dgps -> "dgps"
      | FIX_pps  -> "pps"

    let string_of_date_time (x : date_time) : string =
      match snd x with
      | Some t -> ISO8601.Permissive.string_of_datetimezone (fst x, t)
      | None -> ISO8601.Permissive.string_of_datetime (fst x)

    let wrap_int = fun tag int ->
      Xml.Element (tag, [], [Xml.PCData (string_of_int int)])

    let wrap_float = fun tag float ->
      Xml.Element (tag, [], [Xml.PCData (string_of_float float)])

    let wrap_string = fun tag string ->
      Xml.Element (tag, [], [Xml.PCData string])

    let email x : Xml.xml =
      Xml.Element ("email", [ ("id", x.id) ; ("domain", x.domain) ], [])

    let link x : Xml.xml =
      Xml.Element ("link",
                   [("href", x.href)],
                   opt_apply (wrap_string "text") x.text
                   @@@ opt_apply (wrap_string "type") x.typ
                   @@@ [])

    let wpt (x : wpt) : Xml.xml =
      Xml.Element ("wpt",
                   [ ("lat", string_of_float x.lat) ;
                     ("lon", string_of_float x.lon) ],
                   opt_apply (fun x -> wrap_string "time" (string_of_date_time x))
                             x.time
                   @@@ opt_apply (wrap_float "ele") x.ele
                   @@@ opt_apply (wrap_float "degrees") x.magvar
                   @@@ opt_apply (wrap_float "geoidheight") x.geoidheight
                   @@@ opt_apply (wrap_string "name") x.name
                   @@@ opt_apply (wrap_string "cmt") x.cmt
                   @@@ opt_apply (wrap_string "desc") x.desc
                   @@@ opt_apply (wrap_string "src") x.src
                   @@@ List.map link x.link
                   @ opt_apply (wrap_string "sym") x.sym
                   @@@ opt_apply (wrap_string "typ") x.typ
                   @@@ opt_apply (fun x -> wrap_string "fix" (string_of_fix x))
                                 x.fix
                   @@@ opt_apply (wrap_int "sat") x.sat
                   @@@ opt_apply (wrap_float "vdop") x.vdop
                   @@@ opt_apply (wrap_float "hdop") x.hdop
                   @@@ opt_apply (wrap_float "pdop") x.pdop
                   @@@ opt_apply (wrap_float "ageofdgpsdata") x.ageofdgpsdata
                   @@@ opt_apply (wrap_int "dgpsid") x.dgpsid
                   @@@ x.extensions
                   @@@ [])

    let rtept x = match wpt x with
      | Xml.Element (_, attributes, children) ->
        Xml.Element ("rtept", attributes, children)
      | _ -> assert false

    let trkpt x = match wpt x with
      | Xml.Element (_, attributes, children) ->
        Xml.Element ("trkpt", attributes, children)
      | _ -> assert false

    let trkseg (x : trkseg) : Xml.xml =
      Xml.Element ("trkseg",
                   [],
                   List.map trkpt x.trkpt
                   @ x.extensions
                   @@@ [])

    let bounds (x : bounds) : Xml.xml =
      Xml.Element ("bounds",
                   [ ("minlat", string_of_float x.minlat) ;
                     ("minlon", string_of_float x.minlon) ;
                     ("maxlat", string_of_float x.maxlat) ;
                     ("maxlon", string_of_float x.maxlon) ; ],
                   [])

    let copyright (x : copyright) : Xml.xml =
      Xml.Element ("copyright",
                   [("author", x.author)],
                   opt_apply (wrap_int "year") x.year
                   @@@ opt_apply (wrap_string "license") x.license
                   @@@ [])

    let author (x : person) : Xml.xml =
      Xml.Element ("author",
                   [],
                   opt_apply (wrap_string "name") x.name
                   @@@ opt_apply email x.email
                   @@@ opt_apply link x.link
                   @@@ [])

    let rte (x : rte) : Xml.xml =
      Xml.Element ("rte",
                   [],
                   opt_apply (wrap_string "name") x.name
                   @@@ opt_apply (wrap_string "cmt") x.cmt
                   @@@ opt_apply (wrap_string "desc") x.desc
                   @@@ opt_apply (wrap_string "src") x.src
                   @@@ List.map link x.link
                   @ opt_apply (wrap_int "number") x.number
                   @@@ opt_apply (wrap_string "type") x.typ
                   @@@ x.extensions
                   @@@ List.map rtept x.rtept
                   @ [])

    let trk (x : trk) : Xml.xml =
      Xml.Element ("trk",
                   [],
                   opt_apply (wrap_string "name") x.name
                   @@@ opt_apply (wrap_string "cmt") x.cmt
                   @@@ opt_apply (wrap_string "desc") x.desc
                   @@@ opt_apply (wrap_string "src") x.src
                   @@@ List.map link x.link
                   @ opt_apply (wrap_int "number") x.number
                   @@@ opt_apply (wrap_string "type") x.typ
                   @@@ x.extensions
                   @@@ List.map trkseg x.trkseg
                   @ [])

    let metadata (x : metadata) : Xml.xml =
      Xml.Element ("metadata",
                   [],
                   opt_apply (wrap_string "name") x.name
                   @@@ opt_apply (wrap_string "desc") x.desc
                   @@@ opt_apply author x.author
                   @@@ opt_apply copyright x.copyright
                   @@@ List.map link x.link
                   @ opt_apply
                       (fun x -> wrap_string "time" (string_of_date_time x))
                       x.time
                   @@@ opt_apply (wrap_string "keywords") x.keywords
                   @@@ opt_apply bounds x.bounds
                   @@@ x.extensions
                   @@@ [])

    let gpx x =
      Xml.Element ("gpx",
                   [("version", x.version);
                    ("creator", x.creator)],
                   opt_apply metadata x.metadata
                   @@@ List.map wpt x.wpt
                   @ List.map rte x.rte
                   @ List.map trk x.trk
                   @ x.extensions
                   @@@ [])
end

let of_xml = Of_XML.gpx
let to_xml = To_XML.gpx
