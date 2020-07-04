// Learn more about F# at http://fsharp.org

open System
open TrionfaleLib.TrionfoGame
open TrionfaleLib.Trionfale

let rec finisciMano (m: mano) strategia  =
    if m.carte.Length = 0
    then m
    else finisciMano (aggiornaMano m strategia) strategia

let strategia_so_ismcts (s0: statoGiocatore) =
    let carta = so_ismcts s0 1000
    carta

let giocaMano (model) =
    finisciMano (nuovaManoCasuale()) strategia_so_ismcts

[<EntryPoint>]
let main argv =
  for i = 1 to 100 do
      let mano = giocaMano()
      let trionfa = mano.trionfa.Value.ToString()
      printfn "---"
      printfn "Mano %d" (i+1)
      printfn "Trionfa: %s" (trionfa)
      for p in mano.passate do
        printf "%s" (passataToString p)
      let p02, p13 = punteggioPassate mano.passate mano.trionfa.Value
      printfn "%d - %d" p02 p13
  0
