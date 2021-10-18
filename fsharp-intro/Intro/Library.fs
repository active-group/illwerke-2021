namespace Intro

module Code =
  // Aufzählung
  type Pet = Dog | Cat | Snake

  // Ist Haustier niedlich?
  let isCute (pet: Pet): bool =
    match pet with
    | Dog -> true
    | Cat -> true
    | Snake -> false 

  let isCute' =
   fun (pet: Pet) ->
      match pet with
      | Dog -> true
      | Cat -> true
      | Snake -> false 


  // zusammengesetzte Daten
  // type Time = { Hour: int; Minute: int }
  type Time = { Hour: int 
                Minute: int }

  // Mehrdeutigkeit bzgl. Hour und Minute
  // type PreciseTime = { Hour: int; Minute: int; Seconds: int }

  let time1 = { Hour = 12; Minute = 24 }
  let time2 = { Hour = 5; Minute = 37}

  // Minuten seit Mitternacht
  let msm (time: Time): int =
    time.Hour * 60 + time.Minute

  // Variante
  let msm' ({ Hour = h; Minute = m}): int =
    h * 60 + m

  type Liveness = Dead | Alive

  type Weight = double

  // Ein Tier ist eins der folgenden:
  // - Gürteltier
  // - Papagei
  // algebraischer Datentyp
  // discriminated union
  type Animal =
    | Dillo of Liveness * Weight
    | Parrot of string * Weight

  // Gürteltier, lebendig, 10kg
  let dillo1 = Dillo (Alive, 10.0)
  // totes Gürteltier, 9kg
  let dillo2 = Dillo (Dead, 9.0)

  let parrot1 = Parrot ("Hello!", 1.0)
  let parrot2 = Parrot ("Goodbye!", 1.5)
  
  // Gürteltier überfahren
  let runOverAnimal (animal: Animal): Animal =
    match animal with
    | Dillo (_, w) -> Dillo (Dead, w)
    | Parrot (_, weight) -> Parrot ("", weight)

  // Weight -> (Animal -> Animal)
  let feedAnimal (animal: Animal) (amount: Weight): Animal =
    match animal with
//    | Dillo (liveness, weight) ->
//        match liveness with
//        | Dead -> animal
//        | Alive -> Dillo (liveness, weight + amount)
    | Dillo (Alive, weight) -> Dillo (Alive, weight + amount)
    | Dillo (Dead, _) -> animal
    | Parrot (sentence, weight) -> 
        Parrot (sentence, weight + amount)

  // Langschreibweise von feedAnimal
  let feedAnimal'' =
    fun (animal: Animal) ->
      fun (amount: Weight) ->
        match animal with
        | Dillo (Alive, weight) -> Dillo (Alive, weight + amount)
        | Dillo (Dead, _) -> animal
        | Parrot (sentence, weight) -> 
            Parrot (sentence, weight + amount)

  // Typinferenzalgorithmus: Hindley-Milner-Algorithmus
  // Hindley-Milner-Damas-Algorithmus
  let uncurry (f: 'a -> 'b -> 'c): ('a * 'b) -> 'c =
    fun (a, b) -> (f a) b

  let aTuplify (f: Animal -> Weight -> Animal): (Animal * Weight) -> Animal =
    fun ((animal, weight): Animal * Weight) -> f animal weight

  let aTuplify' f =
    // x hat den Typ 'a
    // y hat den Typ 'b
    fun (x, y) -> f x y

  // Haskell B. Curry
  // Moses Schönfinkel -> "schönfinkeln"
  let curry (f: ('a * 'b) -> 'c): 'a -> 'b -> 'c =
    fun a -> fun b -> f (a, b)

  // ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)
  let flip f b a = f a b

  // fun = lambda
  let flip' (f: 'a -> 'b -> 'c): ('b -> ('a -> 'c)) =
    fun b -> fun a -> f a b

  let feedAnimal' (amount: Weight, animal: Animal): Animal =
    match animal with
    | Dillo (Alive, weight) -> Dillo (Alive, weight + amount)
    | Dillo (Dead, _) -> animal
    | Parrot (sentence, weight) -> 
        Parrot (sentence, weight + amount)

  // Eine geometrische Figur ("shape") ist eins der folgenden:
  // - ein Kreis
  // - ein Quadrat
  // - eine Überlagerung zweier geometrischer Figuren
  // 1. Datenanalyse
  // 2. Funktion: Ist ein Punkt innerhalb einer geometrischen Figur?  (oder außerhalb)


  // Eine Liste ist eins der folgenden:
  // - leere Liste
  // - Cons-Liste aus erstem Element und Rest-Liste
  type List<'a> =
  | Empty
  | Cons of ('a * List<'a>)
 
  // F#:
  // - leere Liste: []
  // - Cons: ::, Infix

  // 'a list = list<'a>
  // int list = list<int>
  let list3: list<int> = 1 :: 2 :: 3 :: []
  let list3' = [1; 2; 3]

  // listSum [1;2;3] = 1 + (2 + (3 + 0))
  let rec listSum (list: list<int>): int =
    match list with
    | [] -> 0
    // Kontext des rekursiven Aufrufs: first + LOCH
    | (first::rest) -> first + listSum rest


  let rec appendToEnd (list: list<'a>) (element: 'a): list<'a> =
    match list with
    | [] -> [element]
    // Kontext des rekursiven Aufrufs: first :: LOCH
    | (first::rest) -> first :: appendToEnd rest element

  (*
(define list-fold
  (lambda (x f list)
    (cond
      ((empty? list) x)
      ((cons? list)
       (f (first list)
          (list-fold x f (rest list)))))))
  *)
  let rec listFold x f list =
    match list with
    | [] -> x
    | (first::rest) -> f first (listFold x f rest)

  let rec rev (list: list<'a>): list<'a> =
    match list with
    | [] -> [] 
    // Kontext: appendToEnd LOCH first
    | (first::rest) -> appendToEnd (rev rest) first

  // CLR: benutzt für Kontexte einen Stack, nur nicht für leere
  // (außer möglicherweise im Debug-Modus)

  let rev' (list0: list<'a>): list<'a> =
      let rec rev1 (list: list<'a>) (acc: list<'a>): list<'a> =
        match list with
        | [] -> acc 
        // Kontext: LOCH
        // tail call
        // endrekursiver Aufruf
        | (first::rest) -> rev1 rest (first::acc)
      rev1 list0 []

  // acc: Summe der bisher schon gesehenen Elemente
  // listSum1 [1;2;3] 0 = ((0 + 1) + 2) + 3
  let rec listSum1 (list: list<int>) (acc: int): int =
    match list with
    // alle Elemente schon gesehen:
    | [] -> acc
    | (first :: rest) -> listSum1 rest (first + acc)  

  let rec listFoldLeft (list: list<int>) (acc: int) f: int =
    match list with
    // alle Elemente schon gesehen:
    | [] -> acc
    | (first :: rest) -> listFoldLeft rest (f first acc) f

   // List.fold: listFoldLeft
   // List.foldBack: listFold


  // type option<'a> = None | Some of 'a
  let rec find (pred: 'a -> bool) (list: list<'a>): option<'a> =
     match list with
     | [] -> None
     | (first::rest) ->
        if pred first
        then Some first
        else find pred rest

  // rev: 1 rekursiver Aufruf pro Cons -> pro Listenelement
  // appendToEnd: 1 rekursiver Aufruf pro Cons / pro Listenelement
  // Liste mit Länger: Anzahl der Aufrufe = n + (n-1) + (n-2) + ... + 2 + 1
  // = (n+1)n/2 = (n^2 + n) / 2 = O(n^2)

module Contracts =
  // 1. einfaches Beispiel erfragen
  // Zero-Coupon Bond / deutsch: Zero-Bond
  // "Ich bekomme am 24.12.2021 100€"
  // 2. in "atomare Bestandteile" zerlegen
  // - "Währung": Ich bekomme jetzt 1€
  // - "Viele": Ich bekomme jetzt 100€
  // - "Später"
  // 3. dabei nach Selbstreferenzen suchen
  // 4. weitere Beispiele
  // Currency swap:
  //    Am 24.12.2021 bekomme ich 100€ und bezahle 80GBP.
  // bisher nur eine Zahlungsrichtung!
  // Idee #1: Multiple mit negativem Betrag

  type Date = string // "2021-12-24"

  type Currency = EUR | GBP | CHF

  type Direction = Long | Short

  type Contract =
  | Zero  // neutrales Element
  | One of Currency
  | Multiple of double * Contract
  | Later of Date * Contract
  // | Direction of Direction * Contract
  | Reverse of Contract // Direction (Short, ...)
  | Union of Contract * Contract  //  <--- sollte man immer suchen

  // Algebra ... Gruppe ...
  // Menge / Typ T
  // val op : (T * T) -> T
  // Assoziativgesetz:
  // op (x, op (y, z)) = op (op (x, y), z)
  // x + (y + z) = (x + y) + z
  // Halbgruppe

  // Kommuntativgesetz:
  // x + y = y + x
  // Halbgruppe + neutrales Element = Monoid

  // Was ist denn das hier:
  // let c''' = Direction (Short, Direction (Short, One EUR ))

  // Ich bekomme 100€ jetzt.
  let contract1 = Multiple (100.0, One EUR) 
  let zcb1 = Later ("2021-12-24", Multiple (100.0, One EUR))

  let zeroCouponBond (date: Date) (amount: double) (currency: Currency): Contract =
     Later (date, Multiple (amount, One currency))


  let zcb1' = zeroCouponBond "2021-12-24" 100.0 EUR

  let cs1 = Union (zeroCouponBond "2021-12-24" 100.0 EUR,
                   Reverse (zeroCouponBond "2021-12-24" 80.0 GBP))

  let currencySwap (date: Date) (amount1: double) (currency1: Currency) (amount2: double) (currency2: Currency): Contract =
     Union (zeroCouponBond date amount1 currency1,
            Reverse (zeroCouponBond date amount2 currency2))
            
  // Multiple 1.0 EUR = One EUR

  // operationelle Semantik

  type Payment = Payment of Direction * Date * double * Currency

  // aktueller Zustand: 
  // - getätigte Zahlungen
  // - Residualvertrag

  // alle Zahlungen bis heute (today) + Residualvertrag
  let calcPayments (contract: Contract) (today: Date): list<Payment> * Contract =
    failwith "Not implemented"

  (*
  // Fehlversuch:
  type Contract =
  | ZeroCouponBond of Date * double * Currency
  | Future
  | CurrencySwap
  | Everest
  | K2
  *)
