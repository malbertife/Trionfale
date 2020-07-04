namespace TrionfaleLib

open IGame
open ISMCTS
open System
open Encog.Neural.Networks
open Encog.Neural.Networks.Layers
open Encog.Engine.Network.Activation
open Encog.ML.Data.Basic
open Encog.Neural.Networks.Training.Propagation.Back
open Encog.Neural.Networks.Training.Propagation.Resilient
open System.IO


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
    
    let indice (c: carta) = 
        ordine c - 1
    
    let statoGiocatoreToArray (ps:statoGiocatore) =
        
        let array = Array.init 84 (fun _ -> 0.0)
        for c in ps.carteInMano do    
            array.[indice c] <- 1.
        for i = 40 to 79 do
            array.[i] <- 1.0
        for c in ps.passate |> List.collect (fun p -> p.carte) do
            array.[40 + indice c] <- 0.
        if ps.trionfa.IsSome then array.[80 + (indiceSeme ps.trionfa.Value)] <- 1.0
        array
      
    let record (ps: statoGiocatore) (value: float) = 
          if not (System.IO.File.Exists("evaluations.csv"))
          then use file = System.IO.File.CreateText("evaluations.csv");
               let intestazioni = Array.init 81 (fun _ -> "")
               for carta in nuovoMazzo() do
                   intestazioni.[indice carta] <- "M" + cartaToStringBreve carta
                   intestazioni.[indice carta + 40] <- "A" + cartaToStringBreve carta
               for i = 0 to 79 do
                   fprintf file "%s;" intestazioni.[i]
               fprintfn file "TB;TC;TD;TS;Score"
          use file = System.IO.File.AppendText("evaluations.csv")
          let valori = statoGiocatoreToArray ps 
          for i = 0 to 84 do
              fprintf file "%.1f;" valori.[i]
          fprintfn file "%.3f" value
    
    //let getModel() =
    //    let model = new Sequential();
    //    model.Add(new Dense(100, input_shape = new Shape(84), activation= "relu"))
    //    model.Add(new Dense(1, activation= "relu"))
    //    model.Compile(loss= "mean_squared_error" , optimizer=new StringOrInstance((new Adam()).ToPython()), metrics=[|"mean_squared_error"|])
    //    model

    //let model = getModel()

    let createNetwork() =
        if File.Exists("nn.eg")
        then Encog.Persist.EncogDirectoryPersistence.LoadObject(new FileInfo("nn.eg")) :?> BasicNetwork
        else let network = BasicNetwork()
             network.AddLayer( BasicLayer( ActivationTANH(), true, 84 ))
             network.AddLayer( BasicLayer( ActivationTANH(), true, 100 ))
             network.AddLayer( BasicLayer( ActivationTANH(), false, 1 ))
             network.Structure.FinalizeStructure()
             network.Reset()
             network
 

    let mutable network = createNetwork()

    let mutable trainingSet = BasicMLDataSet([||], [||])


    let trainer = ResilientPropagation(network, trainingSet)
    trainer.BatchSize <- 1

    let rec so_ismcts (s0: statoGiocatore)  (n: int) =

        
        

        
        let view = { new IGameView<statoGiocatore,mano,carta> with
                     member this.determinize () = determinize s0
                     member this.update_player_state s m = aggiornaStatoGiocatore s m
                     member this.playerState () = s0
                     member this.embed ps = statoGiocatoreToArray ps
                     member this.toPlay () = let p = s0.passate |> List.last
                                             if (p.primo + p.carte.Length) % 2 = 0
                                             then Max
                                             else Min}
        let estimate (ps : statoGiocatore) = 
            let trainingSet = BasicMLDataSet([|view.embed ps |], [|[|0.|]|])
            let item = trainingSet.Item(0)
            let output = network.Compute(item.Input)
            output.[0]
        let fit (ps: statoGiocatore) (value: float) =
            printf "%f %f " (estimate ps) value
            let input = new BasicMLData(view.embed ps)
            let ideal = new BasicMLData([|value|])
            trainingSet.Add(input,ideal)
            trainer.Iteration()
            printfn "%f" trainer.Error
            Encog.Persist.EncogDirectoryPersistence.SaveObject(new System.IO.FileInfo("nn.eg"), network)
        let controller = new is_mcts_controller<statoGiocatore,mano,carta>(view, new Trionfo(), estimate, fit)
        controller.bestMove view n 
    let strategia_so_ismcts (s0: statoGiocatore) =
        let carta = so_ismcts s0 1000
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