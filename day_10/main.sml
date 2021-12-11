datatype chunkType = PAREN | BRACK | BRACE | ANGLE
datatype chunk = T of chunkType * chunk list
datatype parseResult = SUCCESS of chunk list * chunkType option * int
                     | ILLEGAL of chunkType
                     | INCOMPLETE of chunkType list

fun chunk2str (T (ct, children)) =
let
   val childrenStr = concat (map chunk2str children)
in
   case ct of
        PAREN => "(" ^ childrenStr ^ ")"
      | BRACK => "[" ^ childrenStr ^ "]"
      | BRACE => "{" ^ childrenStr ^ "}"
      | ANGLE => "<" ^ childrenStr ^ ">"
end

fun parseChunkList (s, i, parsedChunks) =
   case String.sub (s, i) of
        #"(" => (case parseChunkList (s, i + 1, []) of
                      SUCCESS (children, SOME PAREN, endI) => parseChunkList (
                      s, endI, T (PAREN, children) :: parsedChunks)
                    | SUCCESS (_, SOME ct, _) => ILLEGAL ct
                    | SUCCESS (_, NONE, _) => INCOMPLETE [PAREN]
                    | INCOMPLETE cts => INCOMPLETE (PAREN :: cts)
                    | x => x)
      | #"[" => (case parseChunkList (s, i + 1, []) of
                      SUCCESS (children, SOME BRACK, endI) => parseChunkList (
                      s, endI, T (BRACK, children) :: parsedChunks)
                    | SUCCESS (_, SOME ct, _) => ILLEGAL ct
                    | SUCCESS (_, NONE, _) => INCOMPLETE [BRACK]
                    | INCOMPLETE cts => INCOMPLETE (BRACK :: cts)
                    | x => x)
      | #"{" => (case parseChunkList (s, i + 1, []) of
                      SUCCESS (children, SOME BRACE, endI) => parseChunkList (
                      s, endI, T (BRACE, children) :: parsedChunks)
                    | SUCCESS (_, SOME ct, _) => ILLEGAL ct
                    | SUCCESS (_, NONE, _) => INCOMPLETE [BRACE]
                    | INCOMPLETE cts => INCOMPLETE (BRACE :: cts)
                    | x => x)
      | #"<" => (case parseChunkList (s, i + 1, []) of
                      SUCCESS (children, SOME ANGLE, endI) => parseChunkList (
                      s, endI, T (ANGLE, children) :: parsedChunks)
                    | SUCCESS (_, SOME ct, _) => ILLEGAL ct
                    | SUCCESS (_, NONE, _) => INCOMPLETE [ANGLE]
                    | INCOMPLETE cts => INCOMPLETE (ANGLE :: cts)
                    | x => x)
      | #")" => SUCCESS (rev parsedChunks, SOME PAREN, i + 1)
      | #"]" => SUCCESS (rev parsedChunks, SOME BRACK, i + 1)
      | #"}" => SUCCESS (rev parsedChunks, SOME BRACE, i + 1)
      | #">" => SUCCESS (rev parsedChunks, SOME ANGLE, i + 1)
      | #"\n" => SUCCESS (rev parsedChunks, NONE, i + 1)
      | _ => raise Fail ("unknown character " ^ (str (String.sub (s, i))))

fun str2chunkList s = parseChunkList (s, 0, [])

fun checkParse s =
   case str2chunkList s of
        SUCCESS (chunks, NONE, pos) =>
        if pos = String.size s andalso s = concat (map chunk2str chunks) ^ "\n"
        then print "successfully parsed string\n"
        else print "produced incorrect parse\n"
      | _ => print "failed to parse string\n"

val syntaxErrorScore =
   fn ILLEGAL PAREN => 3
      | ILLEGAL BRACK => 57
      | ILLEGAL BRACE => 1197
      | ILLEGAL ANGLE => 25137
      | _ => 0

val autoCompleteToken =
   fn PAREN => IntInf.fromInt 1
      | BRACK => 2
      | BRACE => 3
      | ANGLE => 4

val autoCompleteScore = foldl (fn (ct, acc) => 5 * acc + autoCompleteToken ct) 0

val () =
   let
      (*
      val s1 = "()\n"
      val s2 = "[]\n"
      val s3 = "([])\n"
      val s4 = "{()()()}\n"
      val s5 = "<([{}])>\n"
      val s6 = "[<>({}){}[([])<>]]\n"
      val s7 = "(((((((((())))))))))\n"
      val () = checkParse s1
      val () = checkParse s2
      val () = checkParse s3
      val () = checkParse s4
      val () = checkParse s5
      val () = checkParse s6
      val () = checkParse s7
      *)
      val inputLines = map str2chunkList (Util.readInputs ())
      val part1 = foldl (op +) 0 (map syntaxErrorScore inputLines)
      val () = print (Int.toString part1 ^ "\n")
      val completionLists =
         List.mapPartial
         (fn INCOMPLETE xs => SOME (rev xs) | _ => NONE)
         inputLines
      val sortedScores =
         Mergesort.sort IntInf.compare (map autoCompleteScore completionLists)
      val part2 = List.nth (sortedScores, length sortedScores div 2)
      val () = print (IntInf.toString part2 ^ "\n")
   in
      ()
   end
