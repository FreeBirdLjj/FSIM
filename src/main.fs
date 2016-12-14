#if INTERACTIVE
# r @"packages/Suave/lib/net40/Suave.dll"
# r @"packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll"
#endif

module Option =
    let inline Bind f x =
        match x with
        | Some x' -> f x'
        | None -> None

let inline internal (>>=) (x : ^T1 option) (f : ^T1 -> ^T2 option) = Option.Bind f x

let internal convertBytesToString : byte array -> string = System.Text.Encoding.UTF8.GetString
let internal convertStringToBytes : string -> byte array = System.Text.Encoding.UTF8.GetBytes

let TryParseJson<'T> (bytes : byte array) : 'T option =
    try
        Some (Suave.Json.fromJson<'T> bytes)
    with
    | ex -> None

let ToJson<'T> : 'T -> byte array = Suave.Json.toJson

let inline GetOptionFromChoice (c : Choice<'T1, 'T2>) : 'T1 option =
    match c with
    | Choice1Of2 cont -> Some cont
    | Choice2Of2 _    -> None

type UserIdType = string

type CSMessageType =
    Login=0 | SendMessage=1 | GetUnreadMessages=2 | GetHistoryMessages=3 | AddFriend=4 | RemoveFriend=5 | GetFriends=6 | Logout=7

type MessagePack = {
    Type : CSMessageType
    Content : string
}

type ClientLoginMessage = {
    Id : UserIdType
    Passwd : string
}

type ClientSendMessage = {
    ReceiverId : UserIdType
    Content : string
}

type ClientAddOrRemoveFriendOrAskHistory = {
    FriendId : UserIdType
}

type ClientMessage private (messageType : CSMessageType, content : Choice<ClientLoginMessage, ClientSendMessage, ClientAddOrRemoveFriendOrAskHistory> option) =
    class
        static member ParseMessage (msg : byte array) : ClientMessage Option =
            TryParseJson<MessagePack> msg
            |> Option.bind (fun messagePack ->
                match messagePack.Type with
                | CSMessageType.Login ->
                    TryParseJson<ClientLoginMessage> (convertStringToBytes messagePack.Content)
                    |> Option.map (fun clientLoginMessage -> ClientMessage(messagePack.Type, Some (Choice1Of3 clientLoginMessage)))
                | CSMessageType.SendMessage ->
                    TryParseJson<ClientSendMessage> (convertStringToBytes messagePack.Content)
                    |> Option.map (fun clientSendMessage -> ClientMessage(messagePack.Type, Some (Choice2Of3 clientSendMessage)))
                | CSMessageType.GetUnreadMessages | CSMessageType.GetFriends | CSMessageType.Logout ->
                    Some (ClientMessage(messagePack.Type, None))
                | CSMessageType.GetHistoryMessages | CSMessageType.AddFriend | CSMessageType.RemoveFriend ->
                    TryParseJson<ClientAddOrRemoveFriendOrAskHistory> (convertStringToBytes messagePack.Content)
                    |> Option.map (fun clientAddOrRemoveFriendOrAskHistory -> ClientMessage(messagePack.Type, Some (Choice3Of3 clientAddOrRemoveFriendOrAskHistory)))
                | _ -> None)

        member val MessageType : CSMessageType = messageType
        member val Content : Choice<ClientLoginMessage, ClientSendMessage, ClientAddOrRemoveFriendOrAskHistory> option = content
    end

type ServerSendMessage = {
    SenderId : UserIdType
    MessageId : int
    Content : string
}

type ServerMessages = {
    Messages : ServerSendMessage array
}

type ServerGetFriends = {
    Friends : UserIdType array
}

let SerializeServerMessage (msgType : CSMessageType) (content : Choice<ServerSendMessage, ServerMessages, ServerGetFriends> option) : byte array =
    let contentJson =
        match content with
        | Some x ->
            match x with
            | Choice1Of3 serverSendMessage -> ToJson serverSendMessage
            | Choice2Of3 serverMessages -> ToJson serverMessages
            | Choice3Of3 serverGetFriends -> ToJson serverGetFriends
        | None -> ""B
    ToJson { Type = msgType; Content = (convertBytesToString contentJson) }

type ChatMessageType = bool * bool * byte array

type MessageCenter private () =
    class

        let lck = obj()
        let mutable messages : ChatMessageType array = Array.zeroCreate 1
        let mutable messageCnt : int = 0

        static member val Instance = MessageCenter()

        member self.Send (msg : byte array) : int =
            lock lck <|
            fun () ->
                if Array.length messages = messageCnt then
                    messages <- Array.append messages (Array.zeroCreate messageCnt)
                messages.[messageCnt] <- (true, false, msg)
                let lastMessageCnt = messageCnt
                messageCnt <- messageCnt + 1
                lastMessageCnt

        member self.GetMessage (msgId : int) : bool * bool * byte array =
            lock lck <| fun () -> messages.[msgId]

        member self.ReadMessage (msgId : int) : byte array =
            lock lck <|
            fun () ->
                let valid, _, content = messages.[msgId]
                messages.[msgId] <- (valid, true, content)
                content

        member self.InvalidMessage (msgId : int) : unit =
            lock lck <|
            fun () ->
                let _, read, content = messages.[msgId]
                messages.[msgId] <- (false, read, content)

        member self.IsMessageValid (msgId : int) : bool =
            lock lck <|
            fun () ->
                let valid, _, _ = messages.[msgId]
                valid

        member self.IsMessageRead (msgId : int) : bool =
            lock lck <|
            fun () ->
                let _, read, _ = messages.[msgId]
                read
    end

type UserContent (passwd : string) =
    class
        member val PerUserLock = obj()
        member val Passwd = passwd
        member val Friends : (UserIdType * bool * int list) list = [] with get, set
    end

type UserCenter private () =
    class

        let lck = obj()
        let mutable users = Map<UserIdType, UserContent>([])

        static member val Instance = UserCenter()

        member self.Register (id : UserIdType) (passwd : string) : bool =
            lock lck <|
            fun () ->
                if users.ContainsKey id then
                    false
                else
                    users <- users.Add (id, UserContent passwd)
                    true

        member self.Login (id : UserIdType) (passwd : string) : bool =
            lock lck (fun () -> users.TryFind id)
            |> Option.map (fun userContent -> lock userContent.PerUserLock (fun () -> userContent.Passwd = passwd))
            |> fun x ->
                match x with
                | Some x' -> x'
                | None -> false

        member self.GetFriends (id : UserIdType) : UserIdType list option =
            lock lck (fun () -> users.TryFind id)
            |> Option.map (fun userContent ->
                lock userContent.PerUserLock (fun () ->
                    userContent.Friends
                    |> List.filter (fun (_, hidden, _) -> not hidden)
                    |> List.map (fun (fid, _, _) -> fid)))

        member self.GetUnreadMessages (id : UserIdType) : (UserIdType * int * byte array) list option =
            lock lck (fun () -> users.TryFind id)
            |> Option.map (fun userContent ->
                lock userContent.PerUserLock (fun () ->
                    userContent.Friends
                    |> List.collect (fun (fid, hidden, msgIds) ->
                        if hidden then
                            []
                        else
                            msgIds
                            |> List.takeWhile (MessageCenter.Instance.IsMessageRead >> not)
                            |> List.filter MessageCenter.Instance.IsMessageValid
                            |> List.map (fun msgId -> (fid, msgId, MessageCenter.Instance.ReadMessage msgId)))))

        member self.GetHistoryMessages (id : UserIdType) (friendId : UserIdType) : (UserIdType * int * byte array) list =
            let findMessagesOneSide (id : UserIdType) (friendId : UserIdType) : (UserIdType * int) list =
                lock lck (fun () -> users.TryFind id)
                |> Option.map (fun userContent ->
                    lock userContent.PerUserLock (fun () ->
                        userContent.Friends
                        |> List.find (fun (fid, _, _) -> fid = friendId)
                        |> fun (_, _, msgIds) -> msgIds
                        |> List.map (fun msgId -> (friendId, msgId))))
                |> fun x ->
                    match x with
                    | Some x' -> x'
                    | None -> []
            let receiveMessages = findMessagesOneSide id friendId
            let sendMessages    = findMessagesOneSide friendId id
            List.append receiveMessages sendMessages
            |> List.sortWith (fun (_, msgId1) (_, msgId2) -> if msgId1 > msgId2 then -1 else if msgId1 < msgId2 then 1 else 0)
            |> List.map (fun (id, msgId) -> (id, msgId, MessageCenter.Instance.ReadMessage msgId))

        member self.AddFriend (id : UserIdType) (friendId : UserIdType) : bool =
            lock lck (fun () -> users.TryFind friendId)
            |> Option.bind (fun _ ->
                lock lck (fun () -> users.TryFind id)
                |> Option.map (fun userContent ->
                    lock userContent.PerUserLock (fun () ->
                        userContent.Friends <- (friendId, false, []) :: userContent.Friends
                        true)))
            |> fun x ->
                match x with
                | Some x' -> x'
                | None -> false

        member self.RemoveFriend (id : UserIdType) (friendId : UserIdType) : bool =
            lock lck (fun () -> users.TryFind id)
            |> Option.map (fun userContent ->
                lock userContent.PerUserLock (fun () ->
                    let hasSuchFriend = userContent.Friends
                                        |> List.exists (fun (fid, hidden, _) -> fid = friendId && not hidden)
                    if hasSuchFriend then
                        userContent.Friends <- userContent.Friends
                                            |> List.map (fun (fid, hidden, msgIds) -> (fid, fid = friendId || hidden, msgIds))
                    hasSuchFriend))
            |> fun x ->
                match x with
                | Some x' -> x'
                | None -> false

        member self.Send (senderId : UserIdType) (receiverId : UserIdType) (receiverOnline : bool) (msg : byte array) : int option =
            lock lck (fun () -> users.TryFind senderId)
            |> Option.bind (fun _ ->
                lock lck (fun () -> users.TryFind receiverId)
                |> Option.map (fun receiverUserContent ->
                    lock receiverUserContent.PerUserLock (fun () ->
                        let hasSuchFriend = receiverUserContent.Friends
                                            |> List.exists (fun (fid, _, _) -> fid = senderId)
                        if not hasSuchFriend then
                            receiverUserContent.Friends <- (senderId, false, []) :: receiverUserContent.Friends
                        let msgId = MessageCenter.Instance.Send msg
                        receiverUserContent.Friends <- receiverUserContent.Friends
                                                    |> List.map (fun (fid, hidden, msgIds) ->
                                                        if fid = senderId then
                                                            if receiverOnline && not hidden then
                                                                MessageCenter.Instance.ReadMessage msgId |> ignore
                                                            (fid, hidden, msgId :: msgIds)
                                                        else
                                                            (fid, hidden, msgIds))
                        msgId)))
    end

type SessionCenter private () =
    class

        let lck = obj()
        let mutable sessions = Map<UserIdType, Session>([])

        static member val Instance = SessionCenter()

        member self.BindSessionWithUser (id : UserIdType) (session : Session) : unit =
            lock lck (fun () -> sessions <- sessions.Add (id, session))

        member self.IsUserOnline (id : UserIdType) : bool =
            lock lck (fun () -> sessions.ContainsKey id)

        member self.GetUserSession (id : UserIdType) : Session option =
            lock lck (fun () -> sessions.TryFind id)

        member self.Logout (id : UserIdType) : unit =
            lock lck (fun () -> sessions <- sessions.Remove id)

        member self.SendMessage (receiverId : UserIdType) (msg : byte array) : bool =
            lock lck (fun () ->
                sessions.TryFind receiverId
                |> Option.map (fun receiverSession -> receiverSession.SendMessageToClient msg))
            |> Option.isSome
    end
and Session() =
    class

        let mutable userId : UserIdType option = None

        member val MessageQueue = new MTS.DataStructure.BlockingQueue<Suave.WebSocket.Opcode * byte array * bool>()

        member self.SendMessageToClient (msg : byte array) : unit =
            self.MessageQueue.Enqueue(Suave.WebSocket.Opcode.Text, msg, true)

        member self.Close() : unit =
            if Option.isSome userId then
                SessionCenter.Instance.Logout userId.Value
            self.MessageQueue.Enqueue(Suave.WebSocket.Opcode.Close, [||], true)

        member self.AuthUser (msg : byte array) : unit =
            ClientMessage.ParseMessage msg
            |> Option.map (fun clientMessage ->
                if clientMessage.MessageType = CSMessageType.Login && Option.isSome clientMessage.Content then
                    match clientMessage.Content.Value with
                    | Choice1Of3 clientLoginMessage ->
                        if UserCenter.Instance.Login clientLoginMessage.Id clientLoginMessage.Passwd then
                            userId <- Some clientLoginMessage.Id
                    | _ -> ())
            |> ignore

        member self.Response (msg : byte array) : unit =
            match userId with
            | Some usrId ->
                ClientMessage.ParseMessage msg
                |> Option.map (fun clientMessage ->
                    let msgType = clientMessage.MessageType
                    match msgType with
                    | CSMessageType.Login -> () // already logined
                    | CSMessageType.SendMessage ->
                        match clientMessage.Content.Value with
                        | Choice2Of3 clientSendMessage ->
                            let receiverId = clientSendMessage.ReceiverId
                            let isReceiverOnline = (SessionCenter.Instance.IsUserOnline receiverId)
                            UserCenter.Instance.Send usrId receiverId isReceiverOnline (convertStringToBytes clientSendMessage.Content)
                            |> Option.map (fun msgId ->
                                self.SendMessageToClient (SerializeServerMessage msgType (Some (Choice1Of3 { SenderId = usrId; MessageId = msgId; Content = "" })))
                                if isReceiverOnline then
                                    SessionCenter.Instance.GetUserSession receiverId
                                    |> Option.map (fun receiverSession ->
                                        receiverSession.SendMessageToClient (SerializeServerMessage msgType (Some (Choice1Of3 { SenderId = usrId; MessageId = msgId; Content = clientSendMessage.Content }))))
                                    |> ignore)
                            |> ignore
                        | _ -> ()
                    | CSMessageType.GetUnreadMessages ->
                        UserCenter.Instance.GetUnreadMessages usrId
                        |> Option.map (fun msgs ->
                            msgs
                            |> List.map (fun (senderId, msgId, content) -> { SenderId = senderId; MessageId = msgId; Content = (convertBytesToString content) })
                            |> List.toArray
                            |> fun messages ->
                                self.SendMessageToClient (SerializeServerMessage msgType (Some (Choice2Of3 { Messages = messages}))))
                        |> ignore
                    | CSMessageType.GetFriends ->
                        UserCenter.Instance.GetFriends usrId
                        |> Option.map (fun friends ->
                            self.SendMessageToClient (SerializeServerMessage msgType (Some (Choice3Of3 { Friends = List.toArray friends }))))
                        |> ignore
                    | CSMessageType.Logout ->
                        self.SendMessageToClient (SerializeServerMessage msgType None)
                        self.Close()
                    | CSMessageType.GetHistoryMessages ->
                        match clientMessage.Content.Value with
                        | Choice3Of3 clientAddOrRemoveFriendOrAskHistory ->
                            let friendId = clientAddOrRemoveFriendOrAskHistory.FriendId
                            UserCenter.Instance.GetHistoryMessages usrId friendId
                            |> List.map (fun (senderId, msgId, content) -> { SenderId = senderId; MessageId = msgId; Content = (convertBytesToString content) })
                            |> List.toArray
                            |> fun historyMessages ->
                                self.SendMessageToClient (SerializeServerMessage msgType (Some (Choice2Of3 { Messages = historyMessages })))
                        | _ -> ()
                    | CSMessageType.AddFriend ->
                        match clientMessage.Content.Value with
                        | Choice3Of3 clientAddOrRemoveFriendOrAskHistory ->
                            let friendId = clientAddOrRemoveFriendOrAskHistory.FriendId
                            if UserCenter.Instance.AddFriend usrId friendId then
                                self.SendMessageToClient (SerializeServerMessage msgType None)
                            SessionCenter.Instance.GetUserSession friendId
                            |> Option.map (fun friendSession ->
                                friendSession.SendMessageToClient (SerializeServerMessage msgType (Some (Choice1Of3 { SenderId = usrId; MessageId = -1; Content = "" }))))
                            |> ignore
                        | _ -> ()
                    | CSMessageType.RemoveFriend ->
                        match clientMessage.Content.Value with
                        | Choice3Of3 clientAddOrRemoveFriendOrAskHistory ->
                            let friendId = clientAddOrRemoveFriendOrAskHistory.FriendId
                            if UserCenter.Instance.RemoveFriend usrId friendId then
                                self.SendMessageToClient (SerializeServerMessage msgType None)
                        | _ -> ()
                    | _ -> ())
                |> ignore
            | None ->
                self.AuthUser msg
                if Option.isSome userId then
                    self.SendMessageToClient (SerializeServerMessage CSMessageType.Login None)
                else
                    self.Close()

        interface System.IDisposable with
            member self.Dispose() : unit =
                (self.MessageQueue :> System.IDisposable).Dispose()
    end

let runloop (webSocket : Suave.WebSocket.WebSocket) (ctx : Suave.Http.HttpContext) =
    Suave.Sockets.Control.SocketMonad.socket {

        use session = new Session()
        use tokenSource = new System.Threading.CancellationTokenSource()

        Async.Start (async {
            try
                while true do

                    let! frame = webSocket.read()
                    GetOptionFromChoice frame
                    |> Option.map (fun msg ->
                        match msg with
                        | (Suave.WebSocket.Opcode.Text, data, true) ->
                            session.Response data
                        | (Suave.WebSocket.Opcode.Close, _, _) ->
                            session.Close()
                        | _ -> ())
                    |> ignore
            with
            | ex -> session.Close()
        }, tokenSource.Token)

        while true do
            let! opcode, msg, fin = async {
                let! frame = session.MessageQueue.AsyncDequeue()
                return Choice1Of2 frame
            }

            do! webSocket.send opcode msg fin

            if opcode = Suave.WebSocket.Opcode.Close then
                tokenSource.Cancel()
                return ()
    }

let registerLoop (webSocket : Suave.WebSocket.WebSocket) (ctx : Suave.Http.HttpContext) =
    Suave.Sockets.Control.SocketMonad.socket {
        let! msg = webSocket.read()

        let registerInfo =
            let successMsg = "Success"B
            let failMsg    = "Fail"B
            match msg with
            | (Suave.WebSocket.Opcode.Text, data, true) ->
                TryParseJson<ClientLoginMessage> data
                |> Option.map (fun clientLoginMessage ->
                    let id = clientLoginMessage.Id
                    let passwd = clientLoginMessage.Passwd
                    if UserCenter.Instance.Register id passwd then
                        successMsg
                    else
                        failMsg)
                |> fun x ->
                    match x with
                    | Some x' -> x'
                    | None -> failMsg
            | _ -> failMsg

        do! webSocket.send Suave.WebSocket.Opcode.Close registerInfo true
    }

let inline internal (>=>) a b = Suave.WebPart.compose a b

let cfg : Suave.SuaveConfig =
    { Suave.Web.defaultConfig with
        bindings =
            [ Suave.Http.HttpBinding.mk Suave.Http.Protocol.HTTP System.Net.IPAddress.Any 3000us;
            //   Suave.Http.HttpBinding.mk (Suave.Http.Protocol.HTTPS Suave.DefaultTlsProvider) System.Net.IPAddress.Any 3001us;
            ]
        maxOps = 10000 }

let app : Suave.Http.WebPart =
    Suave.WebPart.choose [
        Suave.Filters.path "/session" >=> Suave.WebSocket.handShake runloop
        Suave.Filters.path "/register" >=> Suave.WebSocket.handShake registerLoop
        Suave.Filters.path "/" >=> Suave.Files.file "index.html"
        Suave.Filters.path "/index1.html" >=> Suave.Files.file "index1.html"
        Suave.Filters.path "/index2.html" >=> Suave.Files.file "index2.html"
        Suave.Filters.path "/index3.html" >=> Suave.Files.file "index3.html"
        Suave.Filters.path "/index4.html" >=> Suave.Files.file "index4.html"
        Suave.RequestErrors.NOT_FOUND "Found no handlers" ]

Suave.Web.startWebServer cfg app
