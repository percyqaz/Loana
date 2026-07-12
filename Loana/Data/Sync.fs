namespace Loana.Data

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Linq
open System.Security.Cryptography

module Sync =

    let private receive_exact (socket: Socket, length: int) : byte array =
        let buffer: byte array = Array.zeroCreate length
        let mutable read = 0

        while read < length do
            let incoming = socket.Receive(buffer, read, length - read, SocketFlags.None)

            if incoming = 0 then
                failwithf "Socket closed during receive (%i/%i bytes)" read length

            read <- read + incoming

        let hash = Convert.ToHexString(SHA256.HashData(buffer))
        printfn "Finished receiving %i bytes (%s)" length hash
        buffer

    let private send (socket: Socket, data: byte array) : unit =
        let mutable sent = 0
        let hash = Convert.ToHexString(SHA256.HashData(data))
        printfn "Sending %i bytes (%s)" data.Length hash

        while sent < data.Length do
            let outgoing = socket.Send(data, sent, data.Length - sent, SocketFlags.None)

            if outgoing = 0 then
                failwithf "Socket closed during send (%i/%i bytes)" sent data.Length

            sent <- sent + outgoing

    [<Literal>]
    let PORT = 1992

    let SCHEDULE_HEADER = System.Text.Encoding.UTF8.GetBytes("loana-sched-sync")
    let WORDLIST_HEADER = System.Text.Encoding.UTF8.GetBytes("loana-words-sync")

    [<Literal>]
    let MAX_PAYLOAD_BYTES = 4_000_000

    let private receive_payload (socket: Socket, header: byte array) : byte array =
        let received_header = receive_exact(socket, header.Length)

        if not(received_header.SequenceEqual(header)) then
            failwith "Incorrect header"

        let length_bytes =
            IPAddress.NetworkToHostOrder(BitConverter.ToInt32(receive_exact(socket, 4), 0))

        if length_bytes < 0 || length_bytes > MAX_PAYLOAD_BYTES then
            failwith "Invalid payload size"

        receive_exact(socket, length_bytes)

    let private send_payload (socket: Socket, header: byte array, data: Byte array) : unit =
        send(socket, header)
        send(socket, BitConverter.GetBytes(IPAddress.HostToNetworkOrder(data.Length)))
        send(socket, data)

    let private downstream_schedule_sync (socket: Socket, schedule: ReviewSchedule) =
        let schedule_bytes = receive_payload(socket, SCHEDULE_HEADER)

        let schedule_data =
            ReviewScheduleFile.ReadFromStream(new MemoryStream(schedule_bytes))

        let updates = schedule.SyncWith(schedule_data)
        Console.WriteLine(sprintf "Updated %i entries during sync" updates)

    let private upstream_schedule_sync (socket: Socket, schedule: ReviewSchedule) =
        let our_schedule_stream = new MemoryStream()
        ReviewScheduleFile.WriteToStream(schedule.Data, our_schedule_stream)
        let our_schedule_bytes = our_schedule_stream.ToArray()
        our_schedule_stream.Dispose()
        send_payload(socket, SCHEDULE_HEADER, our_schedule_bytes)

    let private downstream_wordlist_sync (socket: Socket, words: WordBank) =
        let words_bytes = receive_payload(socket, WORDLIST_HEADER)
        words.ReadFromStream(new MemoryStream(words_bytes))
        words.WriteToDirectory()
        Console.WriteLine(sprintf "Downloaded %i wordlist entries during sync" words.Entries.Count)

    let private upstream_wordlist_sync (socket: Socket, words: WordBank) =
        let our_words_stream = new MemoryStream()
        words.WriteToStream(our_words_stream)
        let our_words_bytes = our_words_stream.ToArray()
        our_words_stream.Dispose()
        send_payload(socket, WORDLIST_HEADER, our_words_bytes)

    let host (schedule: ReviewSchedule, words: WordBank) : unit =
        let listener =
            new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)

        let start_listening () : unit =
            listener.Bind(IPEndPoint(IPAddress.Any, PORT))
            Console.WriteLine("Listening for a connection..")
            listener.Listen()

        let wait_connection_or_timeout (seconds: float) : Socket option =
            if not(listener.Poll(TimeSpan.FromSeconds(seconds), SelectMode.SelectRead)) then
                Console.WriteLine("Sync cancelled!")
                None
            else
                let client = listener.Accept()
                client.SendTimeout <- 5000
                client.SendBufferSize <- 1_000_000
                client.ReceiveTimeout <- 5000
                client.ReceiveBufferSize <- 1_000_000
                Console.WriteLine("Got a connection!")
                Some client

        let perform_sync (client: Socket) : unit =
            try
                let request = receive_exact(client, 1)

                if request.[0] = 127uy then
                    Console.WriteLine("Syncing schedule..")
                    downstream_schedule_sync(client, schedule)
                    upstream_schedule_sync(client, schedule)

                elif request.[0] = 128uy then
                    Console.WriteLine("Sending wordlists..")
                    upstream_wordlist_sync(client, words)

                else
                    Console.WriteLine("Unknown sync request")

                Console.WriteLine("Sync complete!")
                Threading.Thread.Sleep(1000)
                client.Disconnect(false)
            with _ ->
                client.Dispose()
                reraise()

        try
            try
                start_listening()

                match wait_connection_or_timeout(10.0) with
                | None -> ()
                | Some client -> perform_sync(client)
            with err ->
                Console.WriteLine(err.Message)
                Console.WriteLine(err.StackTrace)
        finally
            listener.Close()
            listener.Dispose()

    let private connect (address: string) : Socket =
        let socket =
            new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)

        Console.WriteLine("Connecting..")
        socket.Connect(address, PORT)
        Console.WriteLine("Connected!")

        socket.SendTimeout <- 5000
        socket.SendBufferSize <- 1_000_000
        socket.ReceiveTimeout <- 5000
        socket.ReceiveBufferSize <- 1_000_000
        socket

    let connect_schedule (schedule: ReviewSchedule, address: string) : unit =
        try
            use socket = connect(address)

            try
                Console.WriteLine("Syncing schedule..")
                send(socket, [| 127uy |])
                upstream_schedule_sync(socket, schedule)
                downstream_schedule_sync(socket, schedule)

                Console.WriteLine("Sync complete!")
                Threading.Thread.Sleep(1000)
                socket.Disconnect(false)
            with _ ->
                socket.Dispose()
                reraise()
        with err ->
            Console.WriteLine(err.Message)
            Console.WriteLine(err.StackTrace)

    let connect_wordlists (words: WordBank, address: string) : unit =
        try
            use socket = connect(address)

            try
                Console.WriteLine("Requesting wordlists..")
                send(socket, [| 128uy |])
                downstream_wordlist_sync(socket, words)

                Console.WriteLine("Sync complete!")
                Threading.Thread.Sleep(1000)
                socket.Disconnect(false)
            with _ ->
                socket.Dispose()
                reraise()
        with err ->
            Console.WriteLine(err.Message)
            Console.WriteLine(err.StackTrace)
