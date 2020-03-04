structure HttpParser =
struct

type headers = unit ptr
type t = (headers * int)
type chunkedDecoder = unit ptr
type request = {method: string, path: string, minorVersion: int, headers: (string * string) list, parsedSize: int}
type response = {minorVersion: int, status: int, message: string, headers: (string * string) list, parsedSize: int}
exception Parse
exception MemoryFull

val phr_prepare_headers = _import "phr_prepare_headers": int -> headers

val phr_prepare_decoder = _import "phr_prepare_decoder": () -> chunkedDecoder

val phr_finalize_headers = _import "free": headers -> ()
val phr_finalize_decoder = _import "free": chunkedDecoder -> ()

val phr_parse_request =
    _import "phr_parse_request":
    (
      string, int,
      char ptr ptr, int ref,
      char ptr ptr, int ref,
      int ref,
      headers, int ref,
      int
    ) -> int

val phr_parse_response =
    _import "phr_parse_response":
    (
      string, int,
      int ref,
      int ref, char ptr ptr, int ref,
      headers, int ref,
      int
    ) -> int

val phr_parse_headers =
    _import "phr_parse_headers":
    (
      string, int,
      headers, int ref,
      int
    ) -> int

val phr_decode_chunked_aux =
    _import "phr_decode_chunked_aux":
    (
      chunkedDecoder,
      CharArray.array, int,
      int ref
    ) -> int

fun prepareHeaders n =
  let
      val headers = phr_prepare_headers n
  in
      if Pointer.isNull headers
      then raise MemoryFull
      else (headers, n)
  end

fun finalizeHeaders (header, i) = (phr_finalize_headers header; ())

local
in
val fromUnitPtr = SMLSharp_Builtin.Pointer.fromUnitPtr
val deref = SMLSharp_Builtin.Pointer.deref
val toUnitPtr = SMLSharp_Builtin.Pointer.toUnitPtr
val advance = Pointer.advance
val importString = Pointer.importString
val isNull = Pointer.isNull
val null = Pointer.NULL
val loadString  = Pointer.importString o Pointer.load
fun getHeader (headers, n) i =
      if 0<=i andalso i < n
      then let
          val header_ptr : char ptr ptr = fromUnitPtr(headers)
          val header_ptr = advance(header_ptr, i * 2)
          val header_ptr : int ptr =
              fromUnitPtr(toUnitPtr(header_ptr))
          val header_ptr = advance(header_ptr , i * 2)

          val header_ptr : char ptr ptr =
              fromUnitPtr(toUnitPtr(header_ptr))
          val name = deref(header_ptr)
          val header_ptr = advance(header_ptr, 1)

          val header_ptr : int ptr =
              fromUnitPtr(toUnitPtr(header_ptr))
          val nameLen = deref(header_ptr)
          val header_ptr = advance(header_ptr, 1)

          val header_ptr : char ptr ptr =
              fromUnitPtr(toUnitPtr(header_ptr))
          val value = deref(header_ptr)
          val header_ptr = advance(header_ptr, 1)

          val header_ptr : int ptr =
              fromUnitPtr(toUnitPtr(header_ptr))
          val valueLen = deref(header_ptr)
      in
          if isNull name
          then (NONE, String.substring(importString(value), 0, valueLen))
          else (SOME(String.substring(importString(name), 0, nameLen)),
                String.substring(importString(value), 0, valueLen))
      end
      else  raise Subscript
end

fun prepareDecoder () =
  let
      val decoder = phr_prepare_decoder()
  in
      if Pointer.isNull decoder
      then raise MemoryFull
      else decoder
  end

fun finalizeDecoder decoder = (phr_finalize_decoder decoder; ())

fun getHeaders headers n =
  let
      fun loop i acc =
        if i = n
        then acc
        else loop (i + 1) ((getHeader headers i) :: acc)

      fun maybeAppend str1 (SOME str2) = str1 ^ str2
        | maybeAppend str1 (NONE) = str1
  in
      #1 (List.foldl (fn ((name, value), (acc, value_acc))=>
                         case name of
                             NONE => (acc, SOME(maybeAppend value value_acc))
                           | SOME name' => ((name', maybeAppend value value_acc):: acc, NONE))
                     ([], NONE) (loop 0 []))
  end

fun parseRequest (t as (headers, n)) buf =
  let
      val bufLen = String.size(buf)
      val method = null ()
      val methodLen = ref 0
      val path = null ()
      val pathLen = ref 0
      val minorVersion = ref 0
      val numHeaders = ref n
  in
      case phr_parse_request(buf, bufLen, method, methodLen, path, pathLen,
                             minorVersion, headers, numHeaders, 0) of
          ~1 => raise Parse
        | ~2 => NONE
        | x => SOME{
                  method = String.substring(loadString method, 0, !methodLen),
                  path = String.substring(loadString path, 0, !pathLen),
                  minorVersion = !minorVersion,
                  headers = getHeaders (headers, n) (!numHeaders),
                  parsedSize = x
              }
  end

fun parseResponse (t as (headers, n)) buf =
  let
      val bufLen = String.size(buf)
      val minorVersion = ref 0
      val status = ref 0
      val msg = null ()
      val msgLen = ref 0
      val numHeaders = ref n
  in
      case phr_parse_response(buf, bufLen, minorVersion, status, msg, msgLen,
                              headers, numHeaders, 0) of
          ~1 => raise Parse
        | ~2 => NONE
        | x  => SOME{
                   minorVersion = !minorVersion,
                   status = !status,
                   message = String.substring(loadString msg, 0, !msgLen),
                   headers = getHeaders t (!numHeaders),
                   parsedSize = x
               }
  end

fun parseHeaders (t as (headers, n)) buf =
  let
      val bufLen = String.size(buf)
      val numHeaders = ref n
  in
      case phr_parse_headers(buf, bufLen, headers, numHeaders, 0) of
          ~1 => raise Parse
        | ~2 => NONE
        | x  => SOME{headers = (getHeaders t (!numHeaders)), parsedSize = x}
  end

fun decodeChunked decoder buf start size =
  case phr_decode_chunked_aux(decoder, buf, start, size) of
      ~1 => raise Parse
    | ~2 => NONE
    | x  => SOME()


end
