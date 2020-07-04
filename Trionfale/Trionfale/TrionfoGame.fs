namespace TrionfaleLib

module TrionfoGame =

    open IGame
    open Carta
    open System



    let peso = function
        | Tre -> 10
        | Due -> 9
        | Asso -> 8
        | Re -> 7
        | Cavallo -> 6
        | Fante -> 5
        | Sette -> 4
        | Sei -> 3
        | Cinque -> 2
        | Quattro -> 1

    let indiceSeme (s0: seme) =
        Array.findIndex (fun s -> s = s0) semi
 
    let ordine (c: carta) =
        10 * (indiceSeme c.seme) + peso c.numero


 
    let valuta (numeri: numero list) =
        numeri.Length + (numeri
                            |> List.sumBy (function
                                        | Asso -> 2
                                        | Due  -> 3
                                        | Tre  -> 4
                                        | Re -> 1
                                        | Cavallo -> 1
                                        | Fante -> 1
                                        | _  -> 0))

    let mettiTrionfa (carte: carta list) = 
        carte
        |> List.groupBy (fun c -> c.seme)
        |> List.maxBy (fun (s, cs) -> cs |> List.map (fun c -> c.numero) |> valuta)
        |> fun (s, _) -> s


    let distribuzione (cl: carta list) =
        let quante = [|0,10;1,10;2,10;3,10|]
        distribuisci cl quante




    let supera (c1: carta) (c2: carta) (trionfa: seme) =
        if c1.seme = c2.seme
        then peso c1.numero > peso c2.numero
        else c1.seme = trionfa



    type mano = {carte: carteInGioco;
                    trionfa: seme option;
                    primo: int;
                    passate: passata list}


    type statoGiocatore = {carteInMano: carta list;
                           passate: passata list;
                           trionfa: seme option}

    let getStato (g: int) (m: mano) = 
           {carteInMano = m.carte 
                        |> List.filter (fun c -> c.giocatore = g)
                        |> List.map (fun c -> c.carta)
            passate = m.passate;
            trionfa = m.trionfa}

    let aChiTocca (m: mano) =
        let p = List.last m.passate
        (p.primo + p.carte.Length) % 4

    let vincitore (p: passata) (trionfa: seme) = 
        let c = p.carte 
                |> List.reduce (fun c1 c2 -> if supera c2 c1 trionfa then c2 else c1)
        let index = List.findIndex (fun c0 -> c0 = c) p.carte
        (p.primo + index) % 4


    let rec aggiornaPassate (trionfa: seme) (c: carta) (ps: passata list) (ultima: bool) =
        match ps with
        | [] -> []
        | [h] -> let h1 = {h with carte = List.append h.carte [c]}
                 if h.carte.Length < 3 || ultima
                 then [h1]
                 else let v = vincitore h1 trionfa
                      [h1; {primo = v; carte = []}]
        | h :: t -> h :: (aggiornaPassate trionfa c t ultima)
   
  
    let aggiornaManoCarta (m: mano) (c: carta) =
        {m with carte =  m.carte |> List.filter (fun cg -> cg.carta <> c);
                passate = aggiornaPassate m.trionfa.Value c m.passate (m.passate.Length = 10)} 
                
    let aggiornaStatoGiocatore (s: statoGiocatore) (c: carta) =
        {s with carteInMano =  s.carteInMano |> List.filter (fun cg -> cg <> c);
                passate = aggiornaPassate s.trionfa.Value c s.passate (s.passate.Length = 10)}

   
    let aggiornaMano (m: mano) (strat: statoGiocatore -> carta) =
        let statoG = getStato (aChiTocca m) m
        let c = strat statoG
        {m with carte =  m.carte |> List.filter (fun cg -> cg.carta <> c);
                passate = aggiornaPassate m.trionfa.Value c m.passate (m.passate.Length = 10)}

    let giocabili (stato:statoGiocatore) =
        match (List.last stato.passate).carte with
        | [] -> stato.carteInMano
        | h::_ -> let stessoSeme = stato.carteInMano |> List.filter (fun c -> c.seme = h.seme)
                  if stessoSeme.Length > 0
                  then stessoSeme
                  else stato.carteInMano

    let strategiaRandom (stato: statoGiocatore) =
        let g = giocabili stato
        List.item (random.Next() % g.Length) g

    let nuovaManoCasuale () =
        let p = random.Next() % 4
        let carte = nuovoMazzo () |> distribuzione
        let cartePrimo = carte |> List.where (fun c -> c.giocatore = p) |> List.map (fun c-> c.carta)
        {carte = carte;
            primo = p;
            trionfa = Some (mettiTrionfa cartePrimo);
            passate = [{primo = p; carte = []}]}





    let punteggio (carte: carta list) =
        carte
        |> List.map (fun c -> match c.numero with
                                | Asso -> 3
                                | Due | Tre | Fante | Cavallo | Re -> 1
                                | _ -> 0)
        |> List.sum
        |> fun c -> int (c / 3)

    let punteggioPassate (ps : passata list) trionfa =
        let vinte02 = ps |> List.filter (fun p -> let v = vincitore p trionfa
                                                  v = 0 || v = 2)
                         |> List.map (fun p -> p.carte)
                         |> List.concat
        let ultima02 = ps |> List.last |> (fun p -> let v = vincitore p trionfa
                                                    v = 0 || v = 2)
        let punteggio02 = punteggio vinte02 + if ultima02 then 1 else 0
        let punteggio13 = 11 - punteggio02 
        punteggio02, punteggio13

    let passataToString (p: passata) =
        let giocate = Array.init 4 (fun i -> i, List.item (if i - p.primo < 0 then i - p.primo + 4 else i - p.primo) p.carte)
        String.Concat([|String.concat " "
                                     (giocate 
                                      |> Array.map (fun (i, c) ->
                                                      String.Concat([| if i = p.primo then ">" else " ";
                                                                       cartaToStringBreve c|])));
                        "\n"|])


  

    let numeroGiocatore (s0: statoGiocatore) =
        let ultima = s0.passate |> List.last 
        (ultima.primo + ultima.carte.Length) % 4

    let determinize (s0: statoGiocatore) =
        let ultima = s0.passate |> List.last 
        let io = (ultima.primo + ultima.carte.Length) % 4
        let giocateInPassata = Array.init 4 (fun _ -> 0)
        for j = 1 to ultima.carte.Length do
            giocateInPassata.[(ultima.primo + j - 1) % 4] <- 1
        let quante = Array.init 4 (fun i -> i, if i = io then 0 else 10 - (s0.passate.Length - 1) - giocateInPassata.[i])
    
        let giocate = s0.passate
                        |> List.collect (fun passata -> passata.carte)
        let viste = List.append s0.carteInMano giocate |> Set.ofList
        let daDistribuire = Set.difference (Set.ofList (nuovoMazzo())) viste 
                            |> Set.toList
        let distribuite = distribuisci daDistribuire quante
        {carte = List.append distribuite (s0.carteInMano |> List.map (fun c -> {carta = c; giocatore = io}))
         trionfa = s0.trionfa
         primo = s0.passate.Head.primo
         passate = s0.passate}

    let isTerminal (m: mano) =
        m.carte.Length = 0


    let available (m: mano) = 
            let statoG = getStato (aChiTocca m) m
            giocabili statoG

    type Trionfo()  =
            interface IGame<statoGiocatore,mano,carta> with
                member this.valid_moves s = available s |> Seq.ofList
                member this.apply_move s m = aggiornaManoCarta s m
                member this.isFinal s = (this :> IGame<statoGiocatore,mano,carta>).valid_moves s |> Seq.length = 0
                member this.score s = 
                    let pari, _ = punteggioPassate s.passate s.trionfa.Value
                    float(2 * pari - 11) / 11.0

