namespace MTS

module AtomicRef =
    [<NoComparison>]
    type AtomicRef<'T> =
        {
            rwLock : obj
            mutable content : 'T
        }

        member self.get() : 'T = lock self.rwLock <| fun () -> self.content
        member self.set(value : 'T) : unit = lock self.rwLock <| fun () -> self.content <- value

        static member ref (value : 'T) : AtomicRef<'T> =
            {
                rwLock = new obj()
                content = value
            }
