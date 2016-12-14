namespace MTS

module AtomicRef =
    [<NoComparison>]
    type AtomicRef<'T> =

        member get : unit -> 'T

        member set : value : 'T -> unit

        static member ref : value : 'T -> AtomicRef<'T> =
