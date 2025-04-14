module Machine

open Elmish
open Elmish.React
open Feliz

type Status =
    | Idle
    | Running of timeRemaining: int
    | Paused of timeRemaining: int

type Model =
    { FloerpLevel: int
      MunLevel: int
      Status: Status
      Output: bool
      Error: string option }

type Msg =
    | AddFloerp of amount: int
    | AddMun of amount: int
    | Start
    | Pause
    | Stop
    | Clear
    | Cycle

let init () =
    { FloerpLevel = 0
      MunLevel = 0
      Status = Idle
      Output = false
      Error = None }
    , Cmd.none

let delayedMsg delay msg dispatch =
    async {
        do! Async.Sleep (delay * 1000)
        dispatch msg
    }
    |> Async.StartImmediate

let update (msg: Msg) model =
    match msg with
    | AddFloerp amount -> 
        { model with 
            Error = None
            FloerpLevel = model.FloerpLevel + amount }
        , Cmd.none
    | AddMun amount ->
        { model with 
            Error = None
            MunLevel = model.MunLevel + amount }
        , Cmd.none
    | Start ->
        match model.Status with
        | Idle when not model.Output && model.FloerpLevel >= 10 && model.MunLevel >= 10 ->
            { model with 
                Error = None
                Status = Running 10 }
            , Cmd.ofEffect (delayedMsg 1 Cycle)
        | Idle when model.Output ->
            { model with Error = Some "Kan niet starten als er nog output is" }
            , Cmd.none
        | Idle when model.FloerpLevel < 10 && model.MunLevel < 10 ->
            { model with Error = Some "Niet genoeg floerp en mun" }
            , Cmd.none
        | Idle when model.FloerpLevel < 10 ->
            { model with Error = Some "Niet genoeg floerp" }
            , Cmd.none
        | Idle -> //when model.MunLevel < 10 ->
            { model with Error = Some "Niet genoeg mun" }
            , Cmd.none
        | Paused time ->
            { model with Status = Running time }
            , Cmd.ofEffect (delayedMsg 1 Cycle)
        | Running _ ->
            model, Cmd.none
    | Pause ->
        match model.Status with
        | Running time ->
            { model with Status = Paused time }
            , Cmd.none
        | _ -> 
            model, Cmd.none
    | Stop ->
        { model with 
            Status = Idle
            Error = None }
        , Cmd.none
    | Clear ->
        { model with Output = false }, Cmd.none
    | Cycle ->
        match model.Status with
        | Running time when time > 1 ->
            { model with Status = Running (time - 1) }
            , Cmd.ofEffect (delayedMsg 1 Cycle)
        | Running time when time <= 1 ->
            { model with 
                FloerpLevel = model.FloerpLevel - 10
                MunLevel = model.MunLevel - 10
                Status = Idle
                Output = true
                Error = None }
            , Cmd.none
        | _ ->
            model, Cmd.none

let view model dispatch =
    Html.div [
        prop.className [ "container" ]

        prop.children [
            Html.main [
                Html.div [
                    prop.classes [ "meters" ]
                    prop.children [
                        Html.div [
                            Html.p [ prop.text "Floerp:" ]
                            Html.p [ prop.text model.FloerpLevel ]
                        ]
                        Html.div [
                            Html.p [ prop.text "Mun:" ]
                            Html.p [ prop.text model.MunLevel ]
                        ]
                        Html.div [
                            Html.p [ prop.text "Output:" ]
                            Html.p [ prop.text (if model.Output then "Beschikbaar" else "Leeg") ]
                        ]
                    ]
                ]
                match model.Error with
                | None -> ()
                | Some err -> 
                    Html.div [
                        prop.classes [ "error" ]
                        prop.text err
                    ]
                Html.div [
                    prop.classes [ "status" ]
                    prop.children (
                        match model.Status with
                        | Running time ->
                            [ Html.p [ prop.text "Bezig" ]
                              Html.p [ prop.text time ] ]
                        | Paused time ->
                            [ Html.p [ prop.text "Pauze" ]
                              Html.p [ prop.text time ] ]
                        | Idle ->
                            [ Html.p [ prop.text "Stand-by" ] ]
                    )
                ]
            ]
            Html.aside [
                Html.button [
                    prop.text "Voeg 5 floerp toe"
                    prop.onClick (fun _ -> dispatch (AddFloerp 5))
                ]
                Html.button [
                    prop.text "Voeg 5 mun toe"
                    prop.onClick (fun _ -> dispatch (AddMun 5))
                ]
                Html.button [
                    prop.text "Start"
                    prop.onClick (fun _ -> dispatch Start)
                ]
                Html.button [
                    prop.text "Pauze"
                    prop.onClick (fun _ -> dispatch Pause)
                ]
                Html.button [
                    prop.text "Stop"
                    prop.onClick (fun _ -> dispatch Stop)
                ]
                Html.button [
                    prop.text "Schoonmaken"
                    prop.onClick (fun _ -> dispatch Clear)
                ]
            ]
        ]
    ]

Program.mkProgram init update view
|> Program.withReactBatched "root"
|> Program.run
