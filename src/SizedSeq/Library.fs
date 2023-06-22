namespace Telefragged

open System
open System.Collections
open System.Collections.Generic

module private Internal =
    [<Struct>]
    [<NoEquality>]
    [<NoComparison>]
    type ArrayBuilder<'T> =
        { mutable currentCount: int
          mutable currentArray: 'T array }

    let inline addToBuilder (item: 'T) (builder: byref<ArrayBuilder<'T>>) =
        match builder.currentCount = builder.currentArray.Length with
        | false ->
            builder.currentArray[builder.currentCount] <- item
            builder.currentCount <- builder.currentCount + 1
        | true ->
            let newArr = Array.zeroCreate (builder.currentCount * 2)
            builder.currentArray.CopyTo(newArr, 0)
            builder.currentArray <- newArr
            newArr[builder.currentCount] <- item
            builder.currentCount <- builder.currentCount + 1

    let inline builderToArray (builder: inref<ArrayBuilder<'T>>) =
        match builder.currentCount = builder.currentArray.Length with
        | true -> builder.currentArray
        | false ->
            let output = Array.zeroCreate builder.currentCount

            for i = 0 to builder.currentCount - 1 do
                output.[i] <- builder.currentArray.[i]

            output

[<Struct>]
type SizeHint =
    | Unknown
    | AtLeast of Atleast: int
    | AtMost of AtMost: int
    | Between of Between: struct (int * int)
    | Exact of Exact: int

module SizeHint =
    let decrement (hint: SizeHint) =
        match hint with
        | Unknown -> Unknown
        | AtLeast n -> AtLeast(n - 1)
        | AtMost n -> AtMost(n - 1)
        | Between(n, m) -> Between(n - 1, m - 1)
        | Exact n -> Exact(n - 1)

    let add (lhs: SizeHint) (rhs: SizeHint) =
        match (lhs, rhs) with
        | (AtMost n, AtMost m) -> AtMost(m + n)
        | (Between(n, _), AtLeast k)
        | (AtLeast k, Between(n, _)) -> AtLeast(n + k)
        | (Between(n, m), AtMost k)
        | (AtMost k, Between(n, m)) -> Between(n, m + k)
        | (Between(n, m), Between(k, l)) -> Between(n + k, m + l)
        | (Exact n, Exact m) -> Exact(n + m)
        | (AtLeast n, AtLeast m)
        | (Exact n, AtLeast m)
        | (AtLeast m, Exact n) -> AtLeast(n + m)
        | (AtLeast n, _)
        | (_, AtLeast n) -> AtLeast(n)
        | (Exact n, _)
        | (_, Exact n) -> AtLeast(n)
        | (_, Unknown)
        | (Unknown, _) -> Unknown

    let upperBounded (hint: SizeHint) =
        match hint with
        | Unknown -> Unknown
        | AtLeast n -> AtMost n
        | AtMost n -> AtMost n
        | Between(n, m) -> AtMost m
        | Exact n -> AtMost n

    let lowerBounded (hint: SizeHint) =
        match hint with
        | AtMost _ -> Unknown
        | Unknown -> Unknown
        | AtLeast n -> AtLeast n
        | Between(n, m) -> AtLeast n
        | Exact n -> AtLeast n

    let isBounded (hint: SizeHint) =
        match hint with
        | AtLeast _
        | Unknown -> false
        | AtMost _
        | Between _
        | Exact _ -> true

type SizedEnumerator<'T>(size: SizeHint, source: IEnumerator<'T>) =
    let mutable sizeHint = size
    let source = source

    member _.MoveNext() =
        sizeHint <- SizeHint.decrement sizeHint
        source.MoveNext()

    member _.Current = source.Current

    member _.Reset() =
        sizeHint <- size
        source.Reset()

    member _.Dispose() = source.Dispose()

    interface IEnumerator<'T> with
        member this.Current = this.Current

    interface IEnumerator with
        member this.Current = this.Current

        member this.MoveNext() = this.MoveNext()

        member this.Reset() = this.Reset()

    interface IDisposable with
        member this.Dispose() = this.Dispose()

[<RequireQualifiedAccess>]
module SizedSeq =
    open Internal

    [<Struct>]
    type SizedSeq<'T> =
        { Source: seq<'T>
          SizeHint: SizeHint }

        member this.GetEnumerator() =
            new SizedEnumerator<'T>(this.SizeHint, this.Source.GetEnumerator())

        interface IEnumerable<'T> with
            member this.GetEnumerator() = this.GetEnumerator()

        interface IEnumerable with
            member this.GetEnumerator() = this.GetEnumerator()

    let private sizeOfSeq (source: seq<'T>) =
        match source with
        | :? ('T[]) as src -> Exact src.Length
        | :? (list<'T>) as src -> Exact src.Length
        | :? (SizedSeq<'T>) as src -> src.SizeHint
        | :? (ICollection<'T>) as src -> Exact src.Count
        | _ -> Unknown

    let sizeHint (source: SizedSeq<'T>) =
        source.SizeHint

    let empty =
        { Source = Seq.empty
          SizeHint = Exact 0 }

    let ofList (source: list<'T>) =
        { Source = source
          SizeHint = Exact source.Length }

    let ofArray (source: 'T[]) =
        { Source = source
          SizeHint = Exact source.Length }

    let ofSeq (source: seq<'T>) =
        { Source = source
          SizeHint = sizeOfSeq source }

    let map (mapping: 'T -> 'U) (source: SizedSeq<'T>) =
        { Source = Seq.map mapping source.Source
          SizeHint = source.SizeHint }

    let filter (predicate: 'T -> bool) (source: SizedSeq<'T>) =
        { Source = Seq.filter predicate source.Source
          SizeHint = SizeHint.upperBounded source.SizeHint }

    let choose (mapping: 'T -> 'U option) (source: SizedSeq<'T>) =
        { Source = Seq.choose mapping source.Source
          SizeHint = SizeHint.upperBounded source.SizeHint }

    let append (lhs: seq<'T>) (rhs: seq<'T>) =
        let lhsSize = sizeOfSeq lhs
        let rhsSize = sizeOfSeq rhs

        { Source = Seq.append lhs rhs
          SizeHint = SizeHint.add lhsSize rhsSize }

    let concat (source: seq<#seq<'T>>) =
        let inline foldSizes sequences =
            Seq.fold (fun acc x -> SizeHint.add acc (sizeOfSeq x)) (Exact 0) sequences

        let sizeHint =
            match source with
            | :? (#seq<'T>[]) as src -> foldSizes src
            | :? (list<#seq<'T>>) as src -> foldSizes src
            // The sequence might be bounded, but the generator might cause side effects
            // which would make iterating over it more than once undesirable.
            // | :? (SizedSeq<#seq<'T>>) as src when SizeHint.isBounded src.SizeHint -> foldSizes src.Source
            | :? (ICollection<seq<'T>>) as src -> foldSizes src
            | _ -> Unknown

        { Source = Seq.concat source
          SizeHint = sizeHint }

    let sortBy (projection: 'T -> 'Key) (source: SizedSeq<'T>) =
        { Source = Seq.sortBy projection source.Source
          SizeHint = source.SizeHint }

    let singleton (value: 'T) =
        { Source = Seq.singleton value
          SizeHint = Exact 1 }

    let init (size: int) (generator: int -> 'T) =
        { Source = Seq.init size generator
          SizeHint = Exact size }

    let toArray (source: SizedSeq<'T>) =
        match source.Source with
        | :? ('T[]) as res -> (res.Clone() :?> 'T[])
        | :? ('T list) as res -> List.toArray res
        | :? ICollection<'T> as res ->
            let arr = Array.zeroCreate res.Count
            res.CopyTo(arr, 0)
            arr
        | _ ->
            use e = source.GetEnumerator()

            if e.MoveNext() then

                let startSize =
                    match source.SizeHint with
                    | AtMost n
                    | AtLeast n
                    | Between(_, n) -> n
                    | Exact n -> n
                    | Unknown -> 4

                let arr = Array.zeroCreate startSize
                arr[0] <- e.Current
                let mutable builder = { currentCount = 1; currentArray = arr }

                while e.MoveNext() do
                    addToBuilder e.Current &builder

                builderToArray &builder
            else
                [||]
