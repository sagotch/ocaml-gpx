(** GPX schema version 1.1 *)

(* For more information on GPX and this schema,
   visit http://www.topografix.com/gpx.asp GPX uses the following conventions:
   all coordinates are relative to the WGS84 datum. All measurements are in
   metric units. *)

(** This library is based on Xml-light.
    Metadata and extensions are represented as Xml.xml,
    from Xml-light library. *)

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

val of_xml : Xml.xml -> gpx
val to_xml : gpx -> Xml.xml

module Of_XML : sig
    val gpx : Xml.xml -> gpx
    val rte : Xml.xml -> rte
    val rtept : Xml.xml -> wpt
    val trk : Xml.xml -> trk
    val trkpt : Xml.xml -> wpt
    val trkseg : Xml.xml -> trkseg
    val wpt : Xml.xml -> wpt
  end

module To_XML : sig
    val gpx : gpx -> Xml.xml
    val rte : rte -> Xml.xml
    val rtept : wpt -> Xml.xml
    val trk : trk -> Xml.xml
    val trkpt : wpt -> Xml.xml
    val trkseg : trkseg -> Xml.xml
    val wpt : wpt -> Xml.xml
end
