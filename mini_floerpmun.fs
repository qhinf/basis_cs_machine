module Machine.Mini

open Elmish
open Elmish.React
open Feliz

type Model =
    { FloerpAvailable: bool
      MunAvailable: bool
      Output: bool
      Error: string option }

type Msg =
    | AddFloerp
    | AddMun
    | Start
    | Clear

let init () =
    { FloerpAvailable = false
      MunAvailable = false
      Output = false
      Error = None }
let update (msg: Msg) model =
    match msg with
    | AddFloerp -> 
        { model with 
            Error = None
            FloerpAvailable = true }
    | AddMun ->
        { model with 
            Error = None
            MunAvailable = true }
    | Start ->
        if not model.Output && model.FloerpAvailable && model.MunAvailable then
            { model with 
                Error = None
                FloerpAvailable = false
                MunAvailable = false
                Output = true }
        elif model.Output then
            { model with Error = Some "Kan niet starten als er nog output is" }
        elif not model.FloerpAvailable && not model.MunAvailable then
            { model with Error = Some "Niet genoeg floerp en mun" }
        elif not model.FloerpAvailable then
            { model with Error = Some "Niet genoeg floerp" }
        else
            { model with Error = Some "Niet genoeg mun" }
    | Clear ->
        { model with Output = false }

let view model dispatch =
    Html.div [
        prop.className [ "container" ]

        prop.children [
            Html.main [
                Html.div [
                    prop.classes [ "meters" ]
                    prop.style [ style.flexDirection.column ]
                    prop.children [
                        Html.div [
                            Html.p [ prop.text "Floerp:" ]
                            Html.p [ prop.text (if model.FloerpAvailable then "Beschikbaar" else "Leeg") ]
                        ]
                        Html.div [
                            Html.p [ prop.text "Mun:" ]
                            Html.p [ prop.text (if model.MunAvailable then "Beschikbaar" else "Leeg") ]
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
            ]
            Html.aside [
                Html.button [
                    prop.text "Voeg floerp toe"
                    prop.onClick (fun _ -> dispatch AddFloerp)
                ]
                Html.button [
                    prop.text "Voeg mun toe"
                    prop.onClick (fun _ -> dispatch AddMun)
                ]
                Html.button [
                    prop.text "Start"
                    prop.onClick (fun _ -> dispatch Start)
                ]
                Html.button [
                    prop.text "Schoonmaken"
                    prop.onClick (fun _ -> dispatch Clear)
                ]
            ]
        ]
    ]

Program.mkSimple init update view
|> Program.withReactBatched "root"
|> Program.run
