(**
 * GPX schema version 1.1 - For more information on GPX and this schema,
 * visit http://www.topografix.com/gpx.asp GPX uses the following conventions:
 * all coordinates are relative to the WGS84 datum. All measurements are in
 * metric units.
 *)

(** metadata and extensions are represented as Xml.xml. *)

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
   extension: extension option;
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
   extentions: extension option;
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
