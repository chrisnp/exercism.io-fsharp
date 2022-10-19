module AtbashCipherTests

open FsUnit.Xunit
open Xunit

open AtbashCipher

[<Fact>]
let ``Encode yes`` () =
    encode "yes" |> should equal "bvh"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Encode no`` () =
    encode "no" |> should equal "ml"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Encode OMG`` () =
    encode "OMG" |> should equal "lnt"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Encode spaces`` () =
    encode "O M G" |> should equal "lnt"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Encode mindblowingly`` () =
    encode "mindblowingly" |> should equal "nrmwy oldrm tob"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Encode numbers`` () =
    encode "Testing,1 2 3, testing." |> should equal "gvhgr mt123 gvhgr mt"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Encode deep thought`` () =
    encode "Truth is fiction." |> should equal "gifgs rhurx grlm"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Encode all the letters`` () =
    encode "The quick brown fox jumps over the lazy dog." |> should equal "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Decode exercism`` () =
    decode "vcvix rhn" |> should equal "exercism"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Decode a sentence`` () =
    decode "zmlyh gzxov rhlug vmzhg vkkrm thglm v" |> should equal "anobstacleisoftenasteppingstone"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Decode numbers`` () =
    decode "gvhgr mt123 gvhgr mt" |> should equal "testing123testing"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Decode all the letters`` () =
    decode "gsvjf rxpyi ldmul cqfnk hlevi gsvoz abwlt" |> should equal "thequickbrownfoxjumpsoverthelazydog"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Decode with too many spaces`` () =
    decode "vc vix    r hn" |> should equal "exercism"

[<Fact(Skip = "Remove this Skip property to run this test")>]
let ``Decode with no spaces`` () =
    decode "zmlyhgzxovrhlugvmzhgvkkrmthglmv" |> should equal "anobstacleisoftenasteppingstone"

