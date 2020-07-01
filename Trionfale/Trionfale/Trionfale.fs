namespace TrionfaleLib

open IGame
open ISMCTS
open System

module Trionfale =

    open Carta
    open TrionfoGame    

    let semeToString (s: seme) = s.ToString().Substring(0,1)
    let numeroToStringBBros = function
      | Asso -> "1"
      | Due -> "2"
      | Tre -> "3"
      | Quattro -> "4" 
      | Cinque -> "5"
      | Sei -> "6"
      | Sette -> "7"
      | Fante -> "8"
      | Cavallo -> "9"
      | Re -> "A"

    let cartaToStringBBros (c: carta) =
          String.Concat [|semeToString c.seme; numeroToStringBBros c.numero|]

    let charToSeme  = function
      | 'B' -> Bastoni
      | 'C' -> Coppe
      | 'D' -> Denari
      | 'S' -> Spade
    let stringToCarta (s: String) =
          let numero = match s.Chars(1) with
          | '1' -> Asso
          | '2' -> Due
          | '3' -> Tre
          | '4' -> Quattro
          | '5' -> Cinque
          | '6' -> Sei
          | '7' -> Sette
          | '8' -> Fante
          | '9' -> Cavallo
          | 'A' -> Re
          {seme = charToSeme (s.Chars(0)); numero = numero}
      



    let rec so_ismcts (s0: statoGiocatore)  (n: int) =
        let controller = new is_mcts_controller<mano,carta>(new Trionfo())
        let view = { new IGameView<mano> with
                     member this.determinize () = determinize s0
                     member this.toPlay () = let p = s0.passate |> List.last
                                             if (p.primo + p.carte.Length) % 2 = 0
                                             then Max
                                             else Min}
        controller.bestMove view n
    let strategia_so_ismcts (s0: statoGiocatore) =
        let carta = so_ismcts s0 2000
        carta
        

    //let giocaMano () =
    //    playout (nuovaManoCasuale()) strategia_so_ismcts


    //let metti (p: Partita) (posto: int) =
    //    let stato = partitaToStatoGiocatore p posto
    //    mettiTrionfa stato.carteInMano |> semeToString
                             
    //let gioca (p: Partita) (posto: int) =
    //    let stato = partitaToStatoGiocatore p posto
    //    let carta = strategia_so_ismcts stato 
    //    carta |> cartaToStringBBros