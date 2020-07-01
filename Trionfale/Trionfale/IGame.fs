module IGame

type Player = Min | Max

type IGame<'TState, 'TMove> =
    abstract member valid_moves: 'TState -> 'TMove seq
    abstract member apply_move: 'TState -> 'TMove -> 'TState
    abstract member score: 'TState -> float
    abstract member isFinal: 'TState -> bool

type IGameView<'TState> =
    abstract member determinize: unit -> 'TState
    abstract member toPlay: unit -> Player