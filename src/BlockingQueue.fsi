namespace MTS

module BlockingQueue =

    [<NoComparison>]
    type internal BlockingQueueAgentMessage<'T> =
    | Put of 'T
    | Get of AsyncReplyChannel<'T>

    /// BlockingQueue for multithreading
    type BlockingQueue<'T> =
        class

            new : unit -> BlockingQueue<'T>

            /// <summary>
            /// Enqueue without blocking current thread, return immediately
            /// </summary>
            member Enqueue : value : 'T -> unit

            member Dequeue : ?timeout : int -> 'T

            member AsyncDequeue : ?timeout : int -> Async<'T>

            interface System.IDisposable
        end
