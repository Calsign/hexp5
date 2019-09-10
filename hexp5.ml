
open P5.Gtkc
open Math
open Vector

let default_n = 14

module Hex : Sketch = struct
  include Base

  type measure = {
    n : int;
    
    window_width : float;
    window_height : float;
    
    board_width : float;
    board_height : float;
    board_left : float;
    board_bottom : float;
    
    hex_side : float;
    theta : float;
    hex_width : float;
    hex_half_width : float;
    hex_height : float;
    hex_int_height : float;
    hex_ext_height : float;
  }

  let build_measure n conf =
    let padding_horiz = 100. in
    let padding_vert = 100. in
    let theta = pi /. 6. in
    let hex_side_from_width =
      (~.(conf.width) -. padding_horiz *. 2.) /. (~.(n + 1) *. 2. /. cos theta) in
    let hex_side_from_height =
      (~.(conf.height) -. padding_vert *. 2.) /. (~.n *. (1. +. sin theta) +. sin theta) in
    let hex_side = minf hex_side_from_width hex_side_from_height in
    let hex_half_width = cos theta *. hex_side in
    let hex_width = hex_half_width *. 2. in
    let hex_int_height = hex_side in
    let hex_ext_height = sin theta *. hex_side in
    let hex_height = hex_int_height +. 2. *. hex_ext_height in
    let board_width = ~.(n - 1) *. hex_width *. 1.5 in
    let board_height = ~.(n - 1) *. (hex_int_height +. hex_ext_height) in
    let board_left = ~.(conf.width) /. 2. -. board_width /. 2. -. hex_half_width in
    let board_bottom = ~.(conf.height) /. 2. +. board_height /. 2.-. hex_ext_height in
    {
      n;
      window_width = ~.(conf.width);
      window_height = ~.(conf.height);
      board_width; board_height;
      board_left; board_bottom;
      hex_side;
      theta;
      hex_width; hex_half_width;
      hex_height; hex_int_height; hex_ext_height;
    }

  type chip = CWhite | CBlack

  module Coord = struct
    type t = int * int
    let compare (x1, y1) (x2, y2) =
      let compare_x = compare x1 x2 in
      if compare_x = 0 then compare y1 y2 else compare_x
  end

  module Coord_map = Map.Make (Coord)

  type move = {
    x : int;
    y : int;
    chip : chip;
    prev_turn : chip;
  }

  type state = {
    measure : measure;
    chip_map : chip Coord_map.t;
    undo : move list;
    redo : move list;
    turn : chip;
  }

  let next_turn = function
    | CWhite -> CBlack
    | CBlack -> CWhite

  let display = `Size (1200, 800)

  let setup conf = {
    measure = build_measure default_n conf;
    chip_map = Coord_map.empty;
    undo = [];
    redo = [];
    turn = CWhite;
  }

  let white = gray 255
  let black = gray 0
  let untaken = gray 127
  let untaken_border = gray 102
  let white_border = gray 204
  let black_border = gray 51

  let color_of_chip = function
    | CWhite -> white
    | CBlack -> black

  let hex (m : measure) (xf : float) (yf : float) =
    poly [
      (xf, yf);
      (xf +. m.hex_half_width, yf -. m.hex_ext_height);
      (xf +. m.hex_width, yf);
      (xf +. m.hex_width, yf +. m.hex_int_height);
      (xf +. m.hex_half_width, yf +. m.hex_int_height +. m.hex_ext_height);
      (xf, yf +. m.hex_int_height);
    ]

  let screen_of_hex_coords (m : measure) x y =
    m.board_left +. ~.x *. m.hex_width +. ~.y *. m.hex_half_width,
    m.board_bottom -. ~.y *. (m.hex_int_height +. m.hex_ext_height)

  let hex_of_screen_coords (m : measure) sx sy =
    let ssx = sx -. m.board_left in
    let ssy = m.board_bottom +. m.hex_int_height +. m.hex_ext_height -. sy in
    let hyi = ssy /. (m.hex_int_height +. m.hex_ext_height) in
    let hy =
      if hyi -. ~.(floor hyi) >
         (m.hex_ext_height /. (m.hex_int_height +. m.hex_ext_height))
      then floor hyi
      else begin
        (* handle the tricky region *)
        let hxiu = ssx /. m.hex_width -. ~.(floor hyi - 1) /. 2. in
        let hxir = hxiu -. ~.(floor hxiu) in
        let hyir = hyi -. ~.(floor hyi) in
        if hxir < 0.5 && hxir > hyir *. 2. ||
           hxir >= 0.5 && (1. -. hxir) > hyir *. 2.
        then floor hyi - 1 (* below *)
        else floor hyi (* above *)
      end in
    let hx = floor (ssx /. m.hex_width -. ~.hy /. 2.) in
    if hx >= 0 && hx < m.n && hy >= 0 && hy < m.n
    then Some (hx, hy) else None

  let positions (m : measure) =
    List.init m.n (fun x -> List.init m.n (fun y -> (x, y))) |> List.flatten

  let board (m : measure) chip_map =
    List.map begin
      fun (x, y) ->
        let xf, yf = screen_of_hex_coords m x y in
        let color = match Coord_map.find_opt (x, y) chip_map with
          | Some chip -> color_of_chip chip
          | None -> untaken in
        hex m xf yf |> fill color
    end (positions m) |> group |> stroke untaken_border

  let borders conf =
    let center = (~.(conf.width), ~.(conf.height)) // 2. in
    let reach = ~.(max conf.width conf.height) *. 2. in
    let theta_tl = pi *. 4. /. 3. in
    let theta_bl = pi *. 5. /. 6. in
    let theta_br = pi *. 7. /. 3. in
    let theta_tr = pi *. 11. /. 6. in
    let build_vec theta = with_mag (of_angle theta) reach in
    group [
      triangle center (center ++ build_vec theta_tl)
        (center ++ build_vec theta_bl) |> fill black_border;
      triangle center (center ++ build_vec theta_tr)
        (center ++ build_vec theta_br) |> fill black_border;
      triangle center (center ++ build_vec theta_tl)
        (center ++ build_vec theta_tr) |> fill white_border;
      triangle center (center ++ build_vec theta_bl)
        (center ++ build_vec theta_br) |> fill white_border;
    ] |> no_stroke

  let draw conf st =
    group [
      borders conf;
      board st.measure st.chip_map;
    ]

  let place_chip st chip x y = {
    st with
    chip_map = Coord_map.add (x, y) chip st.chip_map;
    undo = {x; y; chip; prev_turn = st.turn} :: st.undo;
    redo = [];
  }

  let undo st =
    match st.undo with
    | ({x; y; chip; prev_turn} as move) :: tl ->
      {
        st with
        chip_map = Coord_map.remove (x, y) st.chip_map;
        undo = tl;
        redo = move :: st.redo;
        turn = prev_turn;
      }
    | [] -> st

  let redo st =
    match st.redo with
    | ({x; y; chip; prev_turn} as move) :: tl ->
      {
        st with
        chip_map = Coord_map.add (x, y) chip st.chip_map;
        undo = move :: st.undo;
        redo = tl;
        turn = next_turn prev_turn;
      }
    | [] -> st

  let mouse_pressed conf st =
    match hex_of_screen_coords st.measure ~.(conf.mouse_x) ~.(conf.mouse_y) with
    | Some (x, y) ->
      begin
        (* can't place chips on top of other chips *)
        match Coord_map.find_opt (x, y) st.chip_map with
        | Some _ -> st
        | None ->
          let st' = place_chip st st.turn x y in
          {st' with turn = next_turn st'.turn}
      end
    (* mouse was pressed outside board *)
    | None -> st

  let key_pressed conf st =
    match conf.key_unicode with
    | k when k = KeyUnicode.backspace
          || k = KeyUnicode.left -> undo st
    | k when k = KeyUnicode.right -> redo st
    | _ -> st

  let window_resized conf st =
    {st with measure = build_measure st.measure.n conf}
end

let () = run_sketch (module Hex)
