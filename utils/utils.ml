let read_lines filename =
  In_channel.with_open_text filename In_channel.input_all
  |> String.split_on_char '\n'
