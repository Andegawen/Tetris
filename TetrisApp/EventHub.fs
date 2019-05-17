module Events
open Domain
    type EventHub() = 
        do ()
        member val Subscribers : (Domain.Command->unit) list = list.Empty with get, set
        member this.Subscribe(action:Domain.Command->unit) =
            this.Subscribers<-action::this.Subscribers
        member this.Publish(input:Domain.Command) =
            this.Subscribers |> List.iter (fun s -> s(input))
        