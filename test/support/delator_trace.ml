type capture = {
  buffer : Buffer.t;
  mutex : Mutex.t;
  mutable truncated : bool;
}

let maximum_bytes = 16 * 1024

let append capture text =
  Mutex.lock capture.mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock capture.mutex)
    (fun () ->
      let text_length = String.length text in
      if text_length >= maximum_bytes then (
        Buffer.clear capture.buffer;
        Buffer.add_substring capture.buffer text (text_length - maximum_bytes)
          maximum_bytes;
        capture.truncated <- true;
      ) else (
        let overflow = Buffer.length capture.buffer + text_length - maximum_bytes in
        if overflow > 0 then (
          let retained = Buffer.contents capture.buffer in
          Buffer.clear capture.buffer;
          Buffer.add_substring capture.buffer retained overflow
            (String.length retained - overflow);
          capture.truncated <- true);
        Buffer.add_string capture.buffer text))

let fields fields =
  fields
  |> List.map (fun (name, value) ->
         Printf.sprintf "%s=%S" name (Delator.Field.render value))
  |> String.concat " "

let line capture ~level ~target ~kind ~name logged_fields =
  let rendered_fields = fields logged_fields in
  append capture
    (Printf.sprintf "%s %s %s %s%s\n" (Delator.Level.to_string level) target
       kind name
       (if rendered_fields = "" then "" else " " ^ rendered_fields))

let event message =
  let target = "Outcome_test_support" in
  if Delator.Runtime.is_enabled ~level:Delator.Trace ~target then
    Delator.Runtime.event ~target ~level:Delator.Trace ~msg:message ~fields:[]

let capture action =
  Delator.init ();
  Delator.set_default_level Delator.Trace;
  let state =
    { buffer = Buffer.create 4096; mutex = Mutex.create (); truncated = false }
  in
  let module Renderer = struct
    let on_new_span ~id:_ ~parent:_ ~name ~target ~level ~fields =
      line state ~level ~target ~kind:"span" ~name fields

    let on_exit ~id:_ ~duration_ns:_ = ()

    let on_event ~span:_ ~target ~level ~msg ~fields =
      line state ~level ~target ~kind:"event" ~name:msg fields
  end in
  Delator.Renderer.set_current (module Renderer);
  match action () with
  | value ->
      Delator.Renderer.configure_from_env ();
      let trace = Buffer.contents state.buffer in
      let trace =
        if state.truncated then "[earlier Delator trace omitted]\n" ^ trace else trace
      in
      (value, trace)
  | exception error ->
      let backtrace = Printexc.get_raw_backtrace () in
      Delator.Renderer.configure_from_env ();
      Printexc.raise_with_backtrace error backtrace
