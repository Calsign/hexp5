
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

    positions : (int * int) list;
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
      positions = List.init n (fun x -> List.init n (fun y -> (x, y))) |> List.flatten;
    }

  type chip = CWhite | CBlack | CGray

  module Coord = struct
    type t = int * int
    let compare (x1, y1) (x2, y2) =
      let compare_x = compare x1 x2 in
      if compare_x = 0 then compare y1 y2 else compare_x
  end

  module Coord_map = Map.Make (Coord)
  module Coord_set = Set.Make (Coord)

  type move = {
    x : int;
    y : int;
    chip : chip;
    prev_turn : chip;
  }

  type mode =
    | Game of chip
    | Place of chip

  type state = {
    measure : measure;
    chip_map : chip Coord_map.t;
    undo : move list;
    redo : move list;
    mode : mode;
    winner : chip option;
    n_buffer : string;
  }

  let next_turn = function
    | CWhite -> CBlack
    | CBlack -> CWhite
    | CGray -> CGray

  let display = `Size (1200, 800)

  let build_state n conf = {
    measure = build_measure n conf;
    chip_map = Coord_map.empty;
    undo = [];
    redo = [];
    mode = Game CWhite;
    winner = None;
    n_buffer = "";
  }

  let setup conf =
    build_state default_n conf

  let white = gray 255
  let black = gray 0
  let untaken = gray 127
  let untaken_border = gray 102
  let white_border = gray 204
  let black_border = gray 51

  let color_of_chip = function
    | CWhite -> white
    | CBlack -> black
    | CGray -> untaken_border

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

  let board (m : measure) chip_map =
    List.map begin
      fun (x, y) ->
        let xf, yf = screen_of_hex_coords m x y in
        let color = match Coord_map.find_opt (x, y) chip_map with
          | Some chip -> color_of_chip chip
          | None -> untaken in
        hex m xf yf |> fill color
    end m.positions |> group |> stroke untaken_border

  let borders conf winner =
    let col_black, col_white =
      match winner with
      | Some CWhite -> black_border, white
      | Some CBlack -> black, white_border
      | Some CGray | None -> black_border, white_border in
    let center = (~.(conf.width), ~.(conf.height)) // 2. in
    let reach = ~.(max conf.width conf.height) *. 2. in
    let theta_tl = pi *. 4. /. 3. in
    let theta_bl = pi *. 5. /. 6. in
    let theta_br = pi *. 7. /. 3. in
    let theta_tr = pi *. 11. /. 6. in
    let build_vec theta = with_mag (of_angle theta) reach in
    group [
      triangle center (center ++ build_vec theta_tl)
        (center ++ build_vec theta_bl) |> fill col_black;
      triangle center (center ++ build_vec theta_tr)
        (center ++ build_vec theta_br) |> fill col_black;
      triangle center (center ++ build_vec theta_tl)
        (center ++ build_vec theta_tr) |> fill col_white;
      triangle center (center ++ build_vec theta_bl)
        (center ++ build_vec theta_br) |> fill col_white;
    ] |> no_stroke

  let draw conf st =
    group [
      borders conf st.winner;
      board st.measure st.chip_map;
    ]

  let border_coords n (x, y) =
    [(x + 1, y); (x, y + 1); (x - 1, y + 1); (x - 1, y); (x, y - 1); (x + 1, y - 1)]
    |> List.filter (fun (x', y') -> x' >= 0 && y' >= 0 && x' < n && y < n)

  (* DFS *)
  let rec is_connected (n : int) (chip : chip) (chip_map : chip Coord_map.t)
      (target : Coord_set.t) (visited : Coord_set.t) (start : int * int) =
    let visited' = Coord_set.add start visited in
    (* make sure we're starting from correct-colored tile *)
    if Coord_map.find_opt start chip_map <> Some chip then false, visited'
    (* are we there yet? *)
    else if Coord_set.mem start target then true, visited'
    else begin
      let borders = border_coords n start in
      let valid = borders |> List.filter
                    (* adjacent tiles must be unvisited and correctly-colored *)
                    (fun c -> (not (Coord_set.mem c visited'))
                              && Coord_map.find_opt c chip_map = Some chip) in
      List.fold_left
        (fun (ret, visited'') c ->
           if ret then true, visited''
           else is_connected n chip chip_map target visited'' c)
        (false, visited') valid
    end

  let is_connected_list (n : int) (chip : chip) (chip_map : chip Coord_map.t)
      (target : Coord_set.t) (starts : (int * int) list) =
    List.fold_left
      (fun (ret, visited') c ->
         if ret then true, visited'
         else is_connected n chip chip_map target visited' c)
      (false, Coord_set.empty) starts

  let winner_of_chip_map n chip_map =
    let black_start = List.init n (fun y -> 0, y) in
    let black_target = List.init n (fun y -> n - 1, y) |> Coord_set.of_list in
    let white_start = List.init n (fun x -> x, 0) in
    let white_target = List.init n (fun x -> x, n - 1) |> Coord_set.of_list in
    let black_connected, _ = is_connected_list n CBlack chip_map black_target black_start in
    let white_connected, _ = is_connected_list n CWhite chip_map white_target white_start in
    if black_connected then Some CBlack
    else if white_connected then Some CWhite
    else None

  let place_chip st chip x y =
    let chip_map = Coord_map.add (x, y) chip st.chip_map in
    let prev_turn = match st.mode with
      | Game chip -> chip
      | Place chip -> chip in
    {
      st with
      chip_map;
      undo = {x; y; chip; prev_turn} :: st.undo;
      redo = [];
      winner = winner_of_chip_map st.measure.n chip_map;
    }

  let remove_chip st x y =
    let chip_map = Coord_map.remove (x, y) st.chip_map in
    let undo = List.filter (fun {x=x'; y=y'} -> (x, y) <> (x', y')) st.undo in
    {
      st with
      chip_map;
      undo;
      winner = winner_of_chip_map st.measure.n chip_map;
    }

  let undo st =
    match st.undo with
    | ({x; y; chip; prev_turn} as move) :: tl ->
      let chip_map = Coord_map.remove (x, y) st.chip_map in
      let mode = match st.mode with
        | Game _ -> Game prev_turn
        | Place chip -> Place chip in
      {
        st with
        chip_map;
        undo = tl;
        redo = move :: st.redo;
        mode;
        winner = winner_of_chip_map st.measure.n chip_map;
      }
    | [] -> st

  let redo st =
    match st.redo with
    | ({x; y; chip; prev_turn} as move) :: tl ->
      let chip_map = Coord_map.add (x, y) chip st.chip_map in
      let mode = match st.mode with
        | Game _ -> Game (next_turn prev_turn)
        | Place chip -> Place chip in
      {
        st with
        chip_map;
        undo = move :: st.undo;
        redo = tl;
        mode;
        winner = winner_of_chip_map st.measure.n chip_map;
      }
    | [] -> st

  let mouse_pressed conf st =
    match hex_of_screen_coords st.measure ~.(conf.mouse_x) ~.(conf.mouse_y) with
    | Some (x, y) ->
      begin
        match st.mode, conf.mouse_button, Coord_map.find_opt (x, y) st.chip_map with
        (* left click to place on open tiles *)
        | Game chip, `Left, None ->
          {(place_chip st chip x y) with mode = Game (next_turn chip)}
        | Place chip, `Left, None ->
          place_chip st chip x y
        (* right click to remove *)
        | Place _, `Right, Some _ ->
          remove_chip st x y
        | _ -> st
      end
    (* mouse was pressed outside board *)
    | None -> st

  let is_number c =
    Uchar.to_int c >= (Uchar.of_char '0' |> Uchar.hash)
    && Uchar.to_int c <= (Uchar.of_char '9' |> Uchar.hash)

  let key_pressed conf st =
    match conf.key_unicode with
    | k when k = KeyUnicode.left -> undo st
    | k when k = KeyUnicode.right -> redo st
    | k when k = Uchar.of_char 'b' ->
      {st with mode = Place CBlack}
    | k when k = Uchar.of_char 'w' ->
      {st with mode = Place CWhite}
    | k when k = Uchar.of_char 'x' ->
      {st with mode = Place CGray}
    | k when k = Uchar.of_char 'g' ->
      let mode = match st.mode with
        | Game chip -> Game chip
        | Place CGray -> Game CWhite
        | Place chip -> Game chip in
      {st with mode}
    | k when is_number k ->
      {st with n_buffer = st.n_buffer ^ (Uchar.to_char k |> String.make 1)}
    | k when k = KeyUnicode.backspace ->
      if String.length st.n_buffer > 0
      then {st with n_buffer = String.sub st.n_buffer 0
                        (String.length st.n_buffer - 1)}
      else st
    | k when k = KeyUnicode.enter ->
      begin
        match st.n_buffer |> int_of_string_opt with
        | Some n -> build_state n conf
        | None -> {st with n_buffer = ""}
      end
    | _ -> st

  let window_resized conf st =
    {st with measure = build_measure st.measure.n conf}
end

let () = run_sketch (module Hex)
