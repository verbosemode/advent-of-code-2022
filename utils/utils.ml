let read_lines filename =
  In_channel.with_open_text filename In_channel.input_all
  |> String.split_on_char '\n'
  (* FIXME dafuq. just to drop the empty string at the end? *)
  |> List.filter (fun e -> String.length e <> 0)
