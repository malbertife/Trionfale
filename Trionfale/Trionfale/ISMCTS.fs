namespace TrionfaleLib

module ISMCTS =

    open IGame

    type node<'a> = {mutable data: 'a;
                     mutable children: node<'a> list}

    type mcts_data<'TMove> = {
            move: 'TMove option
            score: float
            visits: float
            }

    let ucb1 score visits parentVisits (k: float) =
        score / visits + k * sqrt (log parentVisits / visits)
            
    type is_mcts_controller<'TState, 'TMove when 'TMove: comparison> (game: IGame<'TState,'TMove>) =
        let game = game

        let newNode (m: 'TMove option) =
            {data = {move = m
                     score = 0.
                     visits = 1e-9}
             children = []}

        let rec playout (v: node<mcts_data<'TMove>>) (state: 'TState) (s: Player)  =  
            if game.isFinal state
            then game.score state 
            else let validMoves = game.valid_moves state |> List.ofSeq
                 let u = Set.difference (validMoves |> Set.ofList)
                                        (v.children |> Seq.map (fun c -> c.data.move.Value) |> Set.ofSeq)
                 if u.Count > 0 then  v.children <- List.append v.children (u
                                                                            |> Set.toList
                                                                            |> List.map (fun m -> newNode (Some m)))
                 let selectedChild = v.children 
                                    |> List.filter (fun c -> List.exists (fun e -> e = c.data.move.Value) validMoves)
                                    |> List.maxBy (fun c -> (ucb1 (if s = Max then c.data.score else -c.data.score)
                                                                  c.data.visits
                                                                  v.data.visits
                                                                  1.0))
                 let reward = playout selectedChild 
                                      (game.apply_move state selectedChild.data.move.Value)
                                      (if s = Max then Min else Max)
                 v.data <- {v.data with score = v.data.score + reward;
                                        visits = v.data.visits + 1.0}
                 reward

        member this.bestMove(view: IGameView<'TState>) (n: int) =
            let v0 = newNode None
            for i = 1 to n do
                let startingState = view.determinize()
                playout v0 startingState (view.toPlay ()) |> ignore;
            v0.children |> List.maxBy (fun c -> c.data.visits) |> fun c -> c.data.move.Value