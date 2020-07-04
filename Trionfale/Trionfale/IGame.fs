module IGame

type Player = Min | Max

type IGame<'TPlayerState, 'TState, 'TMove> =
    abstract member valid_moves: 'TState -> 'TMove seq
    abstract member apply_move: 'TState -> 'TMove -> 'TState
    abstract member score: 'TState -> float
    abstract member isFinal: 'TState -> bool

type IGameView<'TPlayerState,'TState, 'TMove> =
    abstract member playerState: unit -> 'TPlayerState
    abstract member update_player_state: 'TPlayerState -> 'TMove -> 'TPlayerState
    abstract member determinize: unit -> 'TState
    abstract member embed: 'TPlayerState -> float[]
    abstract member toPlay: unit -> Player