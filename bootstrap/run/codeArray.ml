type 'a t = 'a option array

let get cells key =
  if key < 0 || key >= Array.length cells then None else cells.(key)
