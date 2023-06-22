module Tests

open Expecto

open Telefragged

[<Tests>]
let tests =
  testList "samples" [
    test "ofArray returns exact size" {
      // Arrange
      let source = [| 1; 2; 3 |]
      // Act
      let sizedSeq = SizedSeq.ofArray source
      // Assert
      Expect.equal (Exact 3) sizedSeq.SizeHint "SizeHint should be Exact 3"
    }

    test "ofList returns exact size" {
      // Arrange
      let source = [ 1; 2; 3 ]
      // Act
      let sizedSeq = SizedSeq.ofList source
      // Assert
      Expect.equal (Exact 3) sizedSeq.SizeHint "SizeHint should be Exact 3"
    }

    test "ofSeq from SizedSeq returns same size" {
      // Arrange
      let source = SizedSeq.init 3 id
      // Act
      let sizedSeq = SizedSeq.ofSeq source
      // Assert
      Expect.equal (Exact 3) sizedSeq.SizeHint "SizeHint should be Exact 3"
    }

    test "map retains size" {
      // Arrange
      let source = [| 1; 2; 3 |]
      // Act
      let sizedSeq = SizedSeq.map ((+) 1) (SizedSeq.ofArray source)
      // Assert
      Expect.equal (Exact 3) sizedSeq.SizeHint "SizeHint should be Exact 3"
    }

    test "append of two exact sizes returns exact size" {
      // Arrange
      let source1 = [| 1; 2; 3 |]
      let source2 = [ 4; 5; 6 ]
      // Act
      let sizedSeq = SizedSeq.append source1 source2
      // Assert
      Expect.equal (Exact 6) sizedSeq.SizeHint "SizeHint should be Exact 6"
    }

    test "concat of exact sizes returns exact size" {
      // Arrange
      let source1 = SizedSeq.init 3 id :> seq<int>
      let source2 = [| 4; 5; 6 |]
      let source3 = [ 7; 8; 9 ]
      // Act
      let sizedSeq = SizedSeq.concat [ source1; source2; source3 ]
      // Assert
      Expect.equal (Exact 9) sizedSeq.SizeHint "SizeHint should be Exact 6"
    }

    test "iteration returns items" {
      // Arrange
      let source1 = [| 1; 2; 3 |]
      let source2 = [ 4; 5; 6 ]
      // Act
      let sizedSeq = SizedSeq.append source1 source2
      // Assert
      Expect.sequenceEqual (Seq.append source1 source2) sizedSeq "Should return the same items"
    }

    test "toArray returns items" {
      // Arrange
      let source1 = [| 1; 2; 3 |]
      let source2 = [ 4; 5; 6 ]
      // Act
      let sizedSeq = SizedSeq.append source1 source2
      // Assert
      Expect.sequenceEqual (Seq.append source1 source2) (sizedSeq |> SizedSeq.toArray) "Should return the same items"
    }

    test "is lazily evaluated" {
      // Arrange
      let mutable counter = 0
      let source = SizedSeq.init 3 (fun _ -> counter <- counter + 1; counter)
      // Act
      let result = source |> SizedSeq.map ((+) 1) |> SizedSeq.filter ((>) 3)
      // Assert
      Expect.equal 0 counter "Should not have evaluated the source"
      let _ = Seq.head result
      Expect.equal 1 counter "Should have evaluated the source"
    }
  ]
