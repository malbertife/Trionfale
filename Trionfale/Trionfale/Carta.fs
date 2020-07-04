namespace TrionfaleLib

module Carta =


    open System

    type seme = Bastoni | Coppe | Denari | Spade
    type numero = Asso | Due | Tre | Quattro | Cinque | Sei | Sette | Fante | Cavallo | Re

    let semi = [|Bastoni;Coppe;Denari;Spade|]

    let numeri = [|Asso ; Due ; Tre ; Quattro ; Cinque ; Sei ; Sette ; Fante ; Cavallo ; Re|]

    type carta = {
        seme: seme;
        numero: numero
    }

    let cartaToString (c: carta) =
           String.concat " di " [| c.numero.ToString() ; c.seme.ToString()|]

    let cartaToStringBreve (c: carta) =
         let n = match c.numero with 
         | Asso -> "A"
         | Due -> "2"
         | Tre -> "3"
         | Quattro -> "4"
         | Cinque -> "5"
         | Sei -> "6"
         | Sette -> "7"
         | Fante -> "F"
         | Cavallo -> "C"
         | Re -> "R"
         let s = c.seme.ToString().Substring(0,1)
         String.Concat([|n;s|])

    type passata = {primo: int;
                    carte: carta list} 

    
    type cartaInGioco = {carta: carta; giocatore: int} 

    type carteInGioco = cartaInGioco list

    let nuovoMazzo () =
        List.collect (fun seme -> (List.map (fun numero -> {seme = seme; numero = numero}) (Array.toList numeri)  )) (Array.toList semi)

    let random = new Random(Guid.NewGuid().GetHashCode())
   

    let distribuisci (cl: carta list) (quante: (int * int) array) =
        let rec dist (cl1: carta list) = 
            match cl1 with 
            | [] -> []
            | h::t -> 
               let q = quante |> Array.filter (fun (_,e) -> e > 0)
               let l = q.Length
               let i = random.Next() % l
               let g,n = q.[i]
               quante.[Array.findIndex (fun (a,_) -> a = g) quante] <- g, n-1
               {carta = h; giocatore = g} :: dist t
        dist cl