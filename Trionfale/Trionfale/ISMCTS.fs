﻿namespace TrionfaleLib

open System

module ISMCTS =

    open IGame

    type node<'a> = {mutable data: 'a;
                     mutable children: node<'a> list}

    type mcts_data<'TMove> = {
            move: 'TMove option
            prior: float
            reward_sum: float
            visit_count: int
            }


    let value (n: node<mcts_data<'TMove>>) =
        if n.data.visit_count = 0
        then 0.
        else n.data.reward_sum / float(n.data.visit_count)

    let ucb1 score visits parentVisits (k: float) =
        score / visits + k * sqrt (log parentVisits / visits)

    let ucbScore (parent: node<mcts_data<'TMove>>) (child: node<mcts_data<'TMove>>) (player: Player) (k: float) =
        let multiplier = if player = Max then 1. else -1.
        let score = (child.data.prior + value child) * multiplier
        score + k * sqrt (log (float(parent.data.visit_count)) / float(child.data.visit_count + 1))
     
   
            
    type is_mcts_controller<'TPlayerState, 'TState,'TMove when 'TMove: comparison> 
            (view: IGameView<'TPlayerState,'TState, 'TMove>,
             game: IGame<'TPlayerState,'TState,'TMove>,
             estimate: 'TPlayerState -> float,
             record: 'TPlayerState -> float -> unit) =
        let game = game
        let view = view
        let estimate = estimate
        let record = record

        let newNode (ps: 'TPlayerState) (m: 'TMove option) (p: float) =
            {data = {move = m
                     prior = p
                     reward_sum = 0.
                     visit_count = 0}
             children = []}

        let rec playout (v: node<mcts_data<'TMove>>) (playerState: 'TPlayerState) (state: 'TState) (s: Player)   =  
            if game.isFinal state
            then game.score state 
            else let validMoves = game.valid_moves state |> List.ofSeq
                 let u = Set.difference (validMoves |> Set.ofList)
                                        (v.children |> Seq.map (fun c -> c.data.move.Value) |> Set.ofSeq)
                 if u.Count > 0 then  v.children <- List.append v.children (u
                                                                            |> Set.toList
                                                                            |> List.map (fun m ->
                                                                                            let newPlayerState = view.update_player_state playerState m
                                                                                            let prior = estimate newPlayerState 
                                                                                            newNode newPlayerState (Some m) prior))
                 let selectedChild = v.children 
                                    |> List.filter (fun c -> List.exists (fun e -> e = c.data.move.Value) validMoves)
                                    |> List.maxBy (fun c -> ucbScore  v c s 2.0)
                 let move = selectedChild.data.move.Value
                 let reward = playout selectedChild 
                                      (view.update_player_state playerState move)
                                      (game.apply_move state move)
                                      (if s = Max then Min else Max)
                 v.data <- {v.data with reward_sum = v.data.reward_sum + reward;
                                        visit_count = v.data.visit_count + 1}
                 reward

        member this.bestMove (view: IGameView<'TPlayerState,'TState, 'TMove>)
                             (n: int) =
            let ps = view.playerState()
            let v0 = newNode ps None 0.
            for i = 1 to n do
                playout v0
                        ps
                        (view.determinize())
                        (view.toPlay ())
                |> ignore
            let selectedChild = v0.children |> List.maxBy (fun c -> ucbScore  v0 c (view.toPlay ()) 2.0) 
            let value = value v0
            let move = selectedChild.data.move.Value
            record ps value
            move