namespace Loana.Data

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Linq
open System.Security.Cryptography

module Sync =

    let private receive_exact(socket: Socket, length: int) : byte array =
        socket.ReceiveBufferSize <- 1_000_000
        let buffer : byte array = Array.zeroCreate length
        let mutable read = 0

        while read < length do
            let incoming = socket.Receive(buffer, read, length - read, SocketFlags.None)
            if incoming = 0 then failwith "Socket closed during receive"
            read <- read + incoming
            printfn "Received %i bytes, (%i/%i)" incoming read (length - read)
        let hash = Convert.ToHexString(SHA256.HashData(buffer))
        printfn "Finished receiving %i bytes (%s)" length hash
        buffer

    let private send(socket: Socket, data: byte array) : unit =
        socket.SendBufferSize <- 1_000_000
        let mutable sent = 0
        let hash = Convert.ToHexString(SHA256.HashData(data))
        printfn "Sending %i bytes (%s)" data.Length hash
        while sent < data.Length do
            let outgoing = socket.Send(data, sent, data.Length - sent, SocketFlags.None)
            if outgoing = 0 then failwith "Socket closed during send"
            sent <- sent + outgoing
            printfn "Sent %i bytes, (%i/%i)" outgoing sent (data.Length - sent)

    let PORT = 1992
    let SCHEDULE_HEADER = System.Text.Encoding.UTF8.GetBytes("loana-sched-sync")
    let WORDLIST_HEADER = System.Text.Encoding.UTF8.GetBytes("loana-words-sync")
    let MAX_PAYLOAD_BYTES = 4_000_000

    let private receive_payload(socket: Socket, header: byte array) : byte array =
        let received_header = receive_exact(socket, header.Length)
        if not(received_header.SequenceEqual(header)) then failwith "Incorrect header"

        let length_bytes = IPAddress.NetworkToHostOrder(BitConverter.ToInt32(receive_exact(socket, 4), 0))
        if length_bytes < 0 || length_bytes > MAX_PAYLOAD_BYTES then
            failwith "Invalid payload size"

        receive_exact(socket, length_bytes)

    let private send_payload(socket: Socket, header: byte array, data: Byte array) : unit =
        send(socket, header)
        send(socket, BitConverter.GetBytes(IPAddress.HostToNetworkOrder(data.Length)))
        send(socket, data)

    let private downstream_schedule_sync(socket: Socket, schedule: ReviewSchedule) =
        let schedule_bytes = receive_payload(socket, SCHEDULE_HEADER)
        let schedule_data = ReviewScheduleFile.FromStream(new MemoryStream(schedule_bytes))
        let updates = schedule.SyncWith(schedule_data)
        Console.WriteLine(sprintf "Updated %i entries during sync" updates)

    let private upstream_schedule_sync(socket: Socket, schedule: ReviewSchedule) =
        let our_schedule_stream = new MemoryStream()
        ReviewScheduleFile.WriteStream(schedule.Data, our_schedule_stream)
        let our_schedule_bytes = our_schedule_stream.ToArray()
        our_schedule_stream.Dispose()
        send_payload(socket, SCHEDULE_HEADER, our_schedule_bytes)

    let private downstream_wordlist_sync(socket: Socket, words: WordBank) =
        let words_bytes = receive_payload(socket, WORDLIST_HEADER)
        words.ReadPayload(new MemoryStream(words_bytes))
        words.ToDirectory()
        Console.WriteLine(sprintf "Downloaded %i wordlist entries during sync" words.Entries.Count)

    let private upstream_wordlist_sync(socket: Socket, words: WordBank) =
        let our_words_stream = new MemoryStream()
        words.WritePayload(our_words_stream)
        let our_words_bytes = our_words_stream.ToArray()
        our_words_stream.Dispose()
        send_payload(socket, WORDLIST_HEADER, our_words_bytes)

    let host(schedule: ReviewSchedule, words: WordBank) : unit =
        try
            let listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
            listener.Bind(new IPEndPoint(IPAddress.Any, PORT))
            Console.WriteLine("Listening for a connection..")
            listener.Listen()

            if not (listener.Poll(TimeSpan.FromSeconds(30.0), SelectMode.SelectRead)) then
                Console.WriteLine("Connection timed out. Sync cancelled!")
            else
                let client = listener.Accept()
                Console.WriteLine("Got a connection!")
                try
                    client.SendTimeout <- 1000
                    client.ReceiveTimeout <- 1000

                    Console.WriteLine("Syncing schedule..")
                    downstream_schedule_sync(client, schedule)
                    upstream_schedule_sync(client, schedule)
                    Console.WriteLine("Sending wordlists..")
                    upstream_wordlist_sync(client, words)
                    Console.WriteLine("Sync complete!")
                    client.Shutdown(SocketShutdown.Send)
                    client.Close(1000)
                with _ ->
                    client.Close(1000)
                    reraise()

        with err ->
            Console.WriteLine(err.Message)
            Console.WriteLine(err.StackTrace)

    let connect(schedule: ReviewSchedule, words: WordBank, address: string) : unit =
        try
            use socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
            Console.WriteLine("Connecting..")
            socket.Connect(address, PORT)
            Console.WriteLine("Connected!")
            try
                socket.SendTimeout <- 1000
                socket.ReceiveTimeout <- 1000

                Console.WriteLine("Syncing schedule..")
                upstream_schedule_sync(socket, schedule)
                downstream_schedule_sync(socket, schedule)
                Console.WriteLine("Downloading wordlists..")
                downstream_wordlist_sync(socket, words)
                Console.WriteLine("Sync complete!")
                socket.Shutdown(SocketShutdown.Both)
                socket.Close()
            with _ ->
                socket.Close()
                reraise()
        with err ->
            Console.WriteLine(err.Message)
            Console.WriteLine(err.StackTrace)