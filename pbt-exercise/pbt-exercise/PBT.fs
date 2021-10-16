namespace Intro

// Menge von Ints als Liste von disjunkten Intervallen, aufsteigend sortiert
type ISet = list<int * int>

module ISet =

  let rec isValid s =
    match s with
    | ((lo1, hi1) :: (lo2, hi2) :: rest) ->
        // + 1: sonst [(2, 3); (4, 5)] = [(2, 5)] -> brauchen Lücke
        lo1 <= hi1 && hi1 + 1 < lo2 && isValid ((lo2, hi2)::rest)
    | [(lo, hi)] -> lo <= hi
    | [] -> true

  let rec range lo hi =
    if hi < lo 
    then []
    else lo :: (range (lo + 1) hi)

  let rec merge2 l1 l2 =
    match (l1, l2) with
    | ([], l2) -> l2
    | (l1, []) -> l1
    | (x1::l1', x2::l2') ->
      if x1 = x2
      then x1 :: (merge2 l1' l2')
      else if x1 < x2
      then x1 :: (merge2 l1' l2)
      else x2 :: (merge2 l1 l2')

  let rec mmerge l =
    match l with
    | [] -> []
    | x::xs -> merge2 x (mmerge xs)

  let toList s =
      mmerge (List.map (fun (lo, hi) -> range lo hi) s)

  module Arb =
    open FsCheck
    open FsCheck.Util

    let rec dropOverlaps iset =
      match iset with
      | ((_, a) as i)::((b, _) as j) :: rest when a + 1 < b ->
          i :: dropOverlaps (j::rest)
      | (i::_::rest) -> dropOverlaps (i::rest)
      | rest -> rest           

    let iset =
      let gen =
        Gen.map 
          (fun lis -> dropOverlaps 
                       (List.sortBy fst
                         (List.map (fun (a, b) -> (min a b, max a b)) lis)))
          (Arb.toGen (Arb.list (Arb.pair Arb.nonNegativeInt Arb.nonNegativeInt)))
      let shrink = Arb.shrinkList (fun _ -> Seq.empty)
      Arb.fromGenShrink (gen, shrink)

  let rec union s1 s2 =
    failwith "Exercise!"

  module Test =
    open FsCheck
    open FsCheck.Util

    let (.=.) left right = left = right |@ sprintf "%A = %A" left right

    let generatorValid =
      Prop.forAll Arb.iset isValid

    let unionCorrect =
      Prop.forAll (Arb.pair Arb.iset Arb.iset) (fun (s1, s2) ->
        toList (union s1 s2)
        .=.
        merge2 (toList s1) (toList s2))

    let unionValid =
      Prop.forAll (Arb.pair Arb.iset Arb.iset) (fun (s1, s2) ->
        isValid (union s1 s2))

    open NUnit.Framework
    [<Test>]
    let ``dummy so NUnit adapter may find test`` () =
      Assert.AreEqual (1, 1)

    open FsCheck.NUnit

    [<Property>]
    // Funktioniert nicht:
    //let testUnionCorrect = unionCorrect
    // Hinweis: So ist Registrierung des Arbitrary nicht nötig
    let testUnionCorrect () = unionCorrect

    [<Property>]
    let testUnionValid () = unionValid

    [<Property>]
    let testGeneratorValid () = generatorValid
