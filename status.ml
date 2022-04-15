type event = | Left | Right | Middle | Update

(************************
    Utility Functions 
************************)
let run_read_lines prog : (string list, string) result =
    let lines = ref [] in
        let in_channel = Unix.open_process_in prog in
    begin
        try
            while true do lines := input_line in_channel :: !lines done;
        with End_of_file -> ignore (Unix.close_process_in in_channel)
    end;    
    Ok(List.rev !lines)

let run_read prog = 
    match run_read_lines prog with
    | Ok l -> Ok(String.concat "\n" l)
    | Error n -> Error("FAILED")

let regex_first_capture regex str = 
    let regexp = Str.regexp regex in
    Str.search_forward regexp str 0 |> ignore;
    Str.matched_group 1 str

let ( |>> ) v f  = match v with | Ok x -> f v | Error x -> x    
let open_status_win cmd = 
    Unix.system (Printf.sprintf "kitty --name popup-status --hold %s" cmd) |> ignore 


(************************
    Widget Implementation
************************)
let usd event = 
    let output = 
        try
        match run_read "curl -Ss http://www.floatrates.com/daily/usd.json" with
            | Ok std_out -> 
                (regex_first_capture "\"rate\":\\([0-9]+\\.[0-9][0-9]\\)" std_out) |> ( ^ ) "💲"
            | Error _ -> "FAILED"
        with | Not_found -> "FAILED" 
    in
    match event with | _ -> Some output

let volume event = 
    let out = 
        match run_read "pactl get-sink-volume @DEFAULT_SINK@" with
        | Ok std_out ->
            begin
                match regex_first_capture "\\([0-9]*\\)%" std_out |> int_of_string with 
                | vol when (vol > 66) -> "🔊 " ^ (string_of_int vol) ^ "%"
                | vol when (vol > 33) -> "🔉 " ^ (string_of_int vol) ^ "%"
                | vol when (vol > 0) -> "🔈 " ^ (string_of_int vol) ^ "%"
                | _ -> "🔇"
            end
        | Error err -> "Error"
    in
    match event with 
    | Left -> Unix.system "pavucontrol" |> ignore; Some(out)
    | Right | Update | Middle -> Some(out)

let date event =
    let time = Unix.time () |> Unix.localtime in
    let out = Some (Printf.sprintf "🗓️%02i/%02i/%02i" (time.tm_mon + 1) time.tm_mday (time.tm_year mod 100)) in
    match event with
        | Left -> Unix.system "i3-msg -q [instance=\"popup-cale\"] scratchpad show && i3-msg -q [instance=\"popup-cale\"] move position center"  |> ignore; out
        | _ -> out

let updates event =
    let output = 
        try
            begin
                match run_read "checkupdates | wc -l" with
                | Ok "" | Ok "0" -> ""
                | Ok std_out -> "🚩" ^ std_out
                | Error _ -> "FAILED"
            end
        with 
        | Not_found -> "FAILED" 
    in
    match event with | _ -> Some output

let time event =
    let time = Unix.time () |> Unix.localtime in
    let out = Some (Printf.sprintf "🕑%02i:%02i:%02i" time.tm_hour time.tm_min time.tm_sec) in
    match event with
        | Left -> Unix.system "i3-msg -q [instance=\"popup-cale\"] scratchpad show && i3-msg -q [instance=\"popup-cale\"] move position center"  |> ignore; out
        | _ -> out
        
let weather event =
    let out = 
        match run_read_lines "curl -Ss \"https://wttr.in/mitte?u&format=%c%t\\n\"" with
        | Ok lines -> Some (List.hd lines)
        | Error err -> Some "hi"
    in
    match event with 
    | Left -> 
        open_status_win "curl -Ss \"https://wttr.in/mitte?F\"";
        out
    | Right | Update | Middle -> out


(************************
    Program execution
************************)
let () = 
    let event =  
        try 
            begin
                match Sys.getenv "button" with 
                | "1" -> Left
                | "2" -> Middle
                | "3" -> Right
                | _   -> Update
            end
            with Not_found -> Update
    in
    match 
        begin
            (match Sys.argv.(1) with
            | "time" -> time event
            | "date" -> date event
            | "weather" -> weather event
            | "volume" -> volume event
            | "usd" -> usd event
            | "updates" -> updates event
            | _ -> Some (Printf.sprintf "\"%s\" not implemented." Sys.argv.(1)))
        end
    with 
    | Some str -> Printf.printf "%s\n" str
    | None -> Printf.printf "FAILURE"
    |> ignore;
    exit 0