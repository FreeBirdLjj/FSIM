namespace MTS

module BlockingQueue =

    [<NoComparison>]
    type internal BlockingQueueAgentMessage<'T> =
    | Put of 'T
    | Get of AsyncReplyChannel<'T>

    type BlockingQueue<'T>() =
        class
            let putQueue = System.Collections.Generic.Queue<'T>()
            let getQueue = System.Collections.Generic.Queue<AsyncReplyChannel<'T>>()
            let cancellationSource = new System.Threading.CancellationTokenSource()
            let agent = MailboxProcessor<BlockingQueueAgentMessage<'T>>.Start ((fun inbox ->
                async {
                    while true do
                        let! msg = inbox.Receive() in
                        match msg with
                        | Put value ->
                            if getQueue.Count = 0 then
                                putQueue.Enqueue value
                            else
                                let earlistGetReplyChannel = getQueue.Dequeue() in
                                earlistGetReplyChannel.Reply value
                        | Get replyChannel ->
                            if putQueue.Count = 0 then
                                getQueue.Enqueue replyChannel
                            else
                                let earlistPutValue = putQueue.Dequeue() in
                                replyChannel.Reply earlistPutValue
                }), cancellationSource.Token)

            member self.Enqueue (value : 'T) : unit =
                agent.Post(Put value)

            member self.Dequeue(?timeout : int) : 'T =
                agent.PostAndReply((fun replyChannel -> Get replyChannel), ?timeout=timeout)

            member self.AsyncDequeue(?timeout : int) : Async<'T> =
                agent.PostAndAsyncReply((fun replyChannel -> Get replyChannel), ?timeout=timeout)

            interface System.IDisposable with
                member self.Dispose() =
                    cancellationSource.Cancel()
        end
