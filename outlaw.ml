open Sdl;;
include Option;;
exception Already_Written
type 'a write_once = {
  mutable contents : 'a option; 
};;
let make_blank : unit -> 'a write_once = fun () -> {contents = none };;
let set wo n = 
  match wo.contents with 
  | None -> wo.contents <- Some n
  | Some _ -> raise Already_Written
let get wo = get wo.contents;;


let size_of_dot=8.;;
let player_speed=2.;;
let bullet_speed=6.;;
let ticks=ref 0;;
let world_last_deactivation = ref (~- 2000);;
let wall_size = 16.0;;
let wall_distance_from_top  = 96.0;;
type rect_record = {
  mutable x :float;
  mutable y :float; 
  w :float;
  h :float;
};;
let my_exit ()=
  Sdl.quit ();
  print_endline "Thank you for playing";
;;
let initialize   () = 

  init [`EVERYTHING]; 
  at_exit my_exit;
  let window = (Window.create ~title:"Outlaw" ~pos:(`undefined,`undefined)  ~dims:(800,600) ~flags:[Window.OpenGL]) in 
  (
    window ,
    (Render.create_renderer ~win:window ~index:~-1 ~flags:[Render.Software])
  )

;;
let (window , renderer) = initialize ();;
(*let rec flip_byte_rec _ 0 = 0;;*)
(*let rec flip_byte_rec n bit_count = 
  match bit_count with 
  | 0 -> n 
  | bit_count -> 
  let mask = 1 lsl bit_count in 
  let almost_n = flip_byte_rec (n land (lnot mask)) (bit_count-1) in
  let bit = Bool.to_int ((n land mask) <> 0) in
  (almost_n lsl 1) + bit;;

let flip_byte n = flip_byte_rec n 7;;*)
(*let flip_byte n =
  let result = ref 0 in
  for i = 0 to 7 do
    let mask = 1 lsl i in
    let bit = Bool.to_int ((n land mask) <> 0) in
    result := (!result lsl 1) + bit;
  done; 
  !result
;;*)
let flip_byte n = 
  let bit_list = List.init 8 (fun i -> 1 lsl i) in
  let n_bit_list = List.filter (fun x -> ((x land n) <> 0)) bit_list in 
  let n_flip_bit_list = List.map (fun x -> (1 lsl 7) / x) n_bit_list in 
  List.fold_left (+) 0 n_flip_bit_list;;


let flip_byte_list arr = List.map flip_byte arr;;

let fill_rect_f rectf = 
  let xi=int_of_float rectf.x in 
  let yi=int_of_float rectf.y in 
  let wi=int_of_float rectf.w in 
  let hi=int_of_float rectf.h in 
  Render.fill_rect renderer (Rect.make1 (xi,yi,wi,hi))
;;
let sherrif_left_image_data=[
    0b00011000;
    0b00111100;
    0b00011000;
    0b00011000;
    0b00111100;
    0b00111100;
    0b01011010;
    0b10011001;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00101000;
    0b00101000;
    0b00101000;
    0b01000100;
    0b01000100
];;
let sherrif_right_image_data=flip_byte_list sherrif_left_image_data;;


let sherrif_left_walk_image_data=[
    0b00011000;
    0b00111100;
    0b00011000;
    0b00011000;
    0b00111100;
    0b00111100;
    0b01011010;
    0b10011001;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00101000;
    0b00101000;
    0b00010000;
    0b00101000;
    0b01000100
];;

let sherrif_right_walk_image_data=flip_byte_list sherrif_left_walk_image_data;;
let obstacle_image_data  =[
    0b00001001;
    0b00001010;
    0b10001010;
    0b01001100;
    0b00101100;
    0b00011000;
    0b00001001;
    0b00001110;
    0b00001000;
    0b00001000;
    0b10001000;
    0b01001000;
    0b00101000;
    0b00011000;
    0b00011000;
    0b00011000;
];;

let sherrif_left_shoot_down_image_data = [
	0b00011000; 
	0b00111100;
	0b00011000;
	0b00011000;
	0b00010000;
	0b00111000;
	0b00011110;
	0b00011010;
	0b00011001;
	0b00011000;
	0b00011000;
	0b00011000;
	0b00011000;
	0b00011000;
	0b00000100;
	0b01111110;
];;
let sherrif_right_shoot_down_image_data=flip_byte_list sherrif_left_shoot_down_image_data;;
let sherrif_left_shoot_straight_image_data = [
    0b00011000;
    0b00111100;
    0b00011000;
    0b00011000;
    0b00010011;
    0b00111110;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00000100;
    0b01111110;
];;
let sherrif_right_shoot_straight_image_data=flip_byte_list sherrif_left_shoot_straight_image_data;;
let sherrif_left_shoot_up_image_data = [
    0b00011000;
    0b00111100;
    0b00011001;
    0b00011010;
    0b00010110;
    0b00111000;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00000100;
    0b01111110;
];;
let sherrif_right_shoot_up_image_data=flip_byte_list sherrif_left_shoot_up_image_data;;

let sherrif_left_hit_image_data = [
    0b00000000;
    0b01100000;
    0b11110000;
    0b11110000;
    0b01100000;
    0b01100000;
    0b01100000;
    0b01100000;
    0b11100000;
    0b11100000;
    0b11100000;
    0b11100000;
    0b11100000;
    0b10000000;
    0b10000000;
    0b11111111;
  ];;

let sherrif_right_hit_image_data=flip_byte_list sherrif_left_hit_image_data;;
type side_type = Left | Right;;


   

(*let get_status () = Graphics.wait_next_event [Graphics.Poll];;*)


let draw_byte byte x y = 
  for i=0 to 7 do
    let dot_pos_x=x +. (size_of_dot *. (float_of_int i)) in 
    let mask = 0x80 lsr i in
    match (mask land byte) with 
    | 0 -> ()
    | _ -> let rect={x=dot_pos_x;y=y;w=size_of_dot;h=size_of_dot} in fill_rect_f rect


  done;;
let draw_byte_list list x y = ignore (List.mapi (fun i byte -> draw_byte byte x (y +. ((float_of_int i) *. size_of_dot))) list);;
(*let true_dim x = size_of_dot *. x;;*)
class virtual sprite = object (self)
  method virtual draw : unit -> unit
end;;
type up_down_type = Forward | Up | Down 
class obstacle_spr posx posy = object (self)
  val rect={x=posx; y=posy; w=8. *. size_of_dot ;h=16. *. size_of_dot}
  method draw = function () -> 
    Render.set_draw_color renderer ~rgb:(0x88,0x99,0x33) ~a:0xff;
    draw_byte_list obstacle_image_data rect.x rect.y;  
  method get_rect () = rect
end;;
let obstacle = new obstacle_spr (400.0-.32.0)  (300.0+.(wall_distance_from_top/.2.0)-.64.0)
class virtual sherrif_like = object (self) (* I need that because of the circularly recursive types of bullet and sherrif *)
  method virtual get_side : unit -> side_type
  method virtual get_rect : unit -> rect_record
  method virtual unshoot : unit -> unit
  method virtual get_opposing_sherrif : unit -> sherrif_like
  method virtual hit : unit -> unit
end;;

class bullet c s = object (self) 
  val rect={x=(-200.); y=(-200.); w=size_of_dot; h=size_of_dot}
  val color : (int * int * int) = c
  val mutable velx = 0.
  val mutable vely = 0.

  val my_sherrif : sherrif_like = s
  val side : side_type = s#get_side ()
  val side_float = 
  match s#get_side () with
  | Left -> 0.
  | Right -> 1.
  method is_on_screen() = (rect.x > ~-. size_of_dot) && (rect.x < 800. +. rect.w)
  method move () =
    rect.x<-rect.x+.velx;
    rect.y<-rect.y+.vely;
    if ((rect.y < wall_size +. wall_distance_from_top) || (rect.y > 600. -. wall_size-.rect.h)) then vely<-(~-.vely);
    if (not (self#is_on_screen())) then begin velx<-0.; vely<-0.; end;
    let opposing_sherrif = s#get_opposing_sherrif () in
    let opposing_sherrif_r = opposing_sherrif#get_rect () in
    let obstacle_r = obstacle#get_rect () in 
    let hit = if ((rect.x +. size_of_dot > obstacle_r.x) && (rect.x +. size_of_dot < obstacle_r.x +. size_of_dot) && (rect.y < obstacle_r.y +. ( obstacle_r.h)) && (rect.y +. size_of_dot > obstacle_r.y)) then begin
      true
    end else if ((rect.x > obstacle_r.x +. ( obstacle_r.w) -. size_of_dot) && (rect.x < obstacle_r.x +. ( obstacle_r.w)) && (rect.y < obstacle_r.y +. ( obstacle_r.h)) && (rect.y +. size_of_dot > obstacle_r.y)) then begin
      true
    end else if ((rect.y +. size_of_dot > obstacle_r.y) && (rect.y +. size_of_dot < obstacle_r.y +. size_of_dot) && (rect.x < obstacle_r.x +. ( obstacle_r.w)) && (rect.x +. size_of_dot > obstacle_r.x)) then begin
      true
    end else if ((rect.y > obstacle_r.y +. ( obstacle_r.h) -. size_of_dot) && (rect.y < obstacle_r.y +. ( obstacle_r.h)) && (rect.x < obstacle_r.x +. ( obstacle_r.w)) && (rect.x +. size_of_dot > obstacle_r.x)) then begin 
      true  
    end else if ((rect.x +. size_of_dot > opposing_sherrif_r.x) && (rect.x +. size_of_dot < opposing_sherrif_r.x +. size_of_dot) && (rect.y < opposing_sherrif_r.y +. ( opposing_sherrif_r.h)) && (rect.y +. size_of_dot > opposing_sherrif_r.y)) then begin
      opposing_sherrif#hit();
      true
    end else if ((rect.x > opposing_sherrif_r.x +. ( opposing_sherrif_r.w) -. size_of_dot) && (rect.x < opposing_sherrif_r.x +. ( opposing_sherrif_r.w)) && (rect.y < opposing_sherrif_r.y +. ( opposing_sherrif_r.h)) && (rect.y +. size_of_dot > opposing_sherrif_r.y)) then begin
      opposing_sherrif#hit();
      true
      
    end else if ((rect.y +. size_of_dot > opposing_sherrif_r.y) && (rect.y +. size_of_dot< opposing_sherrif_r.y +. size_of_dot) && (rect.x < opposing_sherrif_r.x +. ( opposing_sherrif_r.w)) && (rect.x +. size_of_dot > opposing_sherrif_r.x)) then begin  
      opposing_sherrif#hit();
      true
    end else if ((rect.y > opposing_sherrif_r.y +. ( opposing_sherrif_r.h) -. size_of_dot) && (rect.y < opposing_sherrif_r.y +. ( opposing_sherrif_r.h)) && (rect.x < opposing_sherrif_r.x +. ( opposing_sherrif_r.w)) && (rect.x +. size_of_dot > opposing_sherrif_r.x)) then begin
      opposing_sherrif#hit();
      true
    end else false in 
    (*move the bullet out of bounds if hit something*)
    if hit then begin
          (*move the bullet out of bounds*)
          rect.x <- -200.0;
          rect.y <- -200.0;
          velx<-0.0;
          vely<-0.0;
          
          (*change sherrif to move state*)
          my_sherrif#unshoot();


    end;

  method shoot newx newy direction = 
    rect.x<-newx;
    rect.y<-newy;
    velx <- bullet_speed *. (1. -. (2. *. side_float));
    match direction with    
      | Forward ->  vely <- 0.; 
      | Up -> vely <- ~-. bullet_speed; 
      | Down -> vely <- bullet_speed; 
  method draw () = 
    Render.set_draw_color renderer ~rgb:color ~a:0xff;
    fill_rect_f rect;
end;;
class sherrif c posx posy s = object (self)


  val rect={x=posx; y=posy;w=8.*.size_of_dot;h=16.*.size_of_dot}
  val color : (int * int * int) = c
  val side : side_type = s
  val side_float = 
    match s with
    | Left -> 0.
    | Right -> 1.
  val step_time = 
    match s with
    | Left -> 0
    | Right -> 200
  val opposing_sherrif : sherrif write_once = make_blank ()
  val bullet : bullet write_once = make_blank ()
  (*val mutable image_data = sherrif_get_image_data s*)
  val mutable is_walking = false
  val mutable is_aiming = false
  val mutable is_shooting = false
  val mutable vertical_movement = Forward
  val mutable hits = 0
  val mutable is_hit = false

  method private get_image_data () = 
    let to_step = ((!ticks + step_time) mod 1000) < 500 in
    if (is_hit) then
      match side with 
        | Left -> sherrif_left_hit_image_data
        | Right -> sherrif_right_hit_image_data
    else if (is_aiming||is_shooting) then 
      let a = match vertical_movement with
        | Forward -> [sherrif_left_shoot_straight_image_data;sherrif_right_shoot_straight_image_data]
        | Up -> [sherrif_left_shoot_up_image_data;sherrif_right_shoot_up_image_data]
        | Down -> [sherrif_left_shoot_down_image_data;sherrif_right_shoot_down_image_data]
      in
      match side with 
        | Left -> List.hd a
        | Right -> List.hd (List.tl a)
    else if is_walking && to_step then
      match side with 
        | Left -> sherrif_left_walk_image_data
        | Right -> sherrif_right_walk_image_data
    else 
      match side with 
        | Left -> sherrif_left_image_data
        | Right -> sherrif_right_image_data
  method move velx vely =
    is_hit <- false;
    let bullet = get bullet in
    if (bullet#is_on_screen()) then is_shooting <- true else is_shooting <- false;
    if (not is_shooting) then begin
      vertical_movement <- if (vely>0.0) then Down else if (vely<0.0) then Up else Forward;
      if (not is_aiming) then begin 
        is_walking<-velx<>0. || vely<>0.;
        rect.x<-rect.x+.(velx *. player_speed);
        rect.y<-rect.y+.(vely *. player_speed);
        if (rect.x < 400. *. side_float  ) then rect.x <- 400. *. side_float ;
        if (rect.x > 400. *. (1. +. side_float ) -. rect.w) then rect.x <- 400. *. (1. +. side_float ) -. rect.w;

        if (rect.y < wall_size +. wall_distance_from_top) then rect.y <- wall_size +. wall_distance_from_top;
        if (rect.y > 600. -. wall_size-.rect.h) then rect.y <- 600. -. wall_size -. rect.h;
        (*check to see if the sherrif is trying to move into the obstacle.*)
        let obstacle_r = obstacle#get_rect() in
        if ((rect.x +. rect.w > obstacle_r.x) && (rect.x +. rect.w < obstacle_r.x +. (size_of_dot *. 2.)) && (rect.y < obstacle_r.y +.obstacle_r.h) && (rect.y +.  rect.h > obstacle_r.y)) then
          rect.x <- obstacle_r.x -.  rect.w;
        if ((rect.x > obstacle_r.x +.  obstacle_r.w -. (size_of_dot *. 2.)) && (rect.x < obstacle_r.x +.  obstacle_r.w) && (rect.y < obstacle_r.y +.  obstacle_r.h) && (rect.y +. rect.h > obstacle_r.y))  then
          rect.x <- obstacle_r.x +.  obstacle_r.w;
        if ((rect.y +.  rect.h > obstacle_r.y) && (rect.y +.  rect.h < obstacle_r.y +. (size_of_dot *. 2.)) && (rect.x < obstacle_r.x +.  obstacle_r.w) && (rect.x +. rect.w > obstacle_r.x)) then
          rect.y <- obstacle_r.y -. rect.h;
        if ((rect.y > obstacle_r.y +. obstacle_r.h -. (size_of_dot *. 2.)) && (rect.y < obstacle_r.y +. obstacle_r.h) && (rect.x < obstacle_r.x +. obstacle_r.w) && (rect.x +. rect.w > obstacle_r.x)) then begin
          rect.y <- obstacle_r.y +.  obstacle_r.h;
        end
      end
    end

  method create_bullet () = set bullet (new bullet c  (self  :> sherrif_like))
  method aim () = 
    if (not is_shooting) then is_aiming<-true;
  method shoot_if_aiming () = 
    if (is_aiming) then begin
      is_aiming <- false;
      is_shooting <- true;
      let x_adjust = (1. -. side_float) *. rect.w in
      let y_adjust = size_of_dot *. match vertical_movement with 
      | Up -> 0.
      | Forward -> 3. 
      | Down -> 6.
      
      in

      (get bullet)#shoot (rect.x +. x_adjust) (rect.y +. y_adjust) vertical_movement;
    end

  method draw () =
    Render.set_draw_color renderer ~rgb:color ~a:0xff;
    draw_byte_list (self#get_image_data () ) rect.x rect.y;
  method get_bullet () = get bullet
  method set_opposing_sherrif sher = set opposing_sherrif sher
  method get_rect () = rect
  method unshoot() = is_shooting <- false
  method get_side () = side
  method get_opposing_sherrif () = (get opposing_sherrif :> sherrif_like)
  method hit () =    
    hits <- hits + 1;
    is_hit <- true;
    world_last_deactivation := !ticks;
  method gethits () = hits
end;;


let sherrif1 = new sherrif (0x66,0x33,0x99) 200. 200. Left;;
let sherrif2 = new sherrif (0x33,0x99,0x66) 600. 400. Right;;
sherrif1#set_opposing_sherrif sherrif2;;
sherrif2#set_opposing_sherrif sherrif1;;
sherrif1#create_bullet();; sherrif2#create_bullet();;

let all_sprites : sprite list = [(sherrif1 :> sprite);(sherrif2 :> sprite);(obstacle :> sprite);(sherrif1#get_bullet () :> sprite);(sherrif2#get_bullet () :> sprite)];;

let naught=[
    0b01111110;
    0b11111111;
    0b11000011;
    0b11000011;
    0b11000011;
    0b11000011;
    0b11000011;
    0b11000011;
    0b11000011;
    0b11000011;
    0b11111111;
    0b01111110;
];;
let one = [
    0b00111100;
    0b00111100;
    0b00001100;
    0b00001100;
    0b00001100;
    0b00001100;
    0b00001100;
    0b00001100;
    0b00001100;
    0b00001100;
    0b00001100;
    0b00000000;
];;
let two = [
    0b11111111;
    0b11111111;
    0b00000011;
    0b00000011;
    0b00000011;
    0b11111111;
    0b11111111;
    0b11000000;
    0b11000000;
    0b11000000;
    0b11111111;
    0b11111111;
];;
let three = [
    0b11111111;
    0b11111111;
    0b00000011;
    0b00000011;
    0b00000011;
    0b11111111;
    0b11111111;
    0b00000011;
    0b00000011;
    0b00000011;
    0b11111111;
    0b11111111;
];;
let four=[
    0b00011000;
    0b00111000;
    0b01111000;
    0b11011000;
    0b10011000;
    0b11111111;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00011000;
    0b00011000;
];;
let five=[
    0b11111111;
    0b11111111;
    0b11000000;
    0b11000000;
    0b11000000;
    0b11111111;
    0b11111111;
    0b00000011;
    0b00000011;
    0b00000011;
    0b11111111;
    0b11111111;
];;
let six= [
    0b11111111;
    0b11111111;
    0b11000000;
    0b11000000;
    0b11000000;
    0b11111111;
    0b11111111;
    0b11000011;
    0b11000011;
    0b11000011;
    0b11111111;
    0b11111111;
];;
let seven= [
    0b11111111;
    0b11111111;
    0b00000011;
    0b00000110;
    0b00001100;
    0b11111111;
    0b11111111;
    0b00110000;
    0b00110000;
    0b01100000;
    0b01100000;
    0b11000000;
];;
let eight=[
    0b11111111;
    0b11111111;
    0b11000011;
    0b11000011;
    0b11000011;
    0b11111111;
    0b11111111;
    0b11000011;
    0b11000011;
    0b11000011;
    0b11111111;
    0b11111111;
];;
let nine =[
    0b11111111;
    0b11111111;
    0b11000011;
    0b11000011;
    0b11000011;
    0b11111111;
    0b11111111;
    0b00000011;
    0b00000011;
    0b00000011;
    0b00000011;
    0b00000011;
    ];;
let ten = [
    0b11011111;
    0b11011111;
    0b11011011;
    0b11011011;
    0b11011011;
    0b11011011;
    0b11011011;
    0b11011011;
    0b11011011;
    0b11011011;
    0b11011111;
    0b11011111;
];;
let digits = [naught; one; two; three; four; five; six; seven; eight; nine; ten];;
let draw_hits () =
  (*draw Player 1's score*)
  
  let hits1 = sherrif2#gethits () in
  Render.set_draw_color renderer ~rgb:(0x66,0x33,0x99) ~a:0xff;
  draw_byte_list (List.nth digits hits1) (16. *. size_of_dot) 0.;
  (*draw player 2's score*)

  let hits2 = sherrif1#gethits () in
  Render.set_draw_color renderer ~rgb:(0x33,0x99,0x66) ~a:0xff;
  draw_byte_list (List.nth digits hits2) (800. -. 64. -. (16. *. size_of_dot)) 0.;;



let draw () =
  Render.set_draw_color renderer ~rgb:(0xf0,0xf0,0xe7) ~a:0xff;
  Render.clear renderer;
  let wall_size_int = int_of_float wall_size in 
  let wall_top_rect=Rect.make1 (0,int_of_float wall_distance_from_top,800,wall_size_int) in
  let wall_bottom_rect=Rect.make1 (0,600-wall_size_int,800,wall_size_int) in

  Render.set_draw_color renderer ~rgb:(140,120,100) ~a:0xff;
  Render.fill_rect renderer wall_top_rect;
  Render.fill_rect renderer wall_bottom_rect;
  let scoreboard_rect=Rect.make1 (0,0,800,int_of_float wall_distance_from_top ) in 
  Render.set_draw_color renderer ~rgb:(0xaa,0xaa,0xbb) ~a:0xff;
  Render.fill_rect renderer scoreboard_rect;

  List.iter (fun s -> s#draw ()) all_sprites;
  draw_hits ();
  Render.render_present renderer;
;;

let rec get_event_list () = 
  let e=Event.poll_event () in
  match e with 
    | None -> []
    | Some x -> x::(get_event_list ())
;;
(*let get_event_seq () = Seq.unfold (fun () -> (Event.poll_event ()) (fun e' -> Some (e', ())) ) ();;
let get_event_list () = List.of_seq (get_event_seq ());;*)
type important_key_record = {
  mutable w: bool; 
  mutable a: bool;
  mutable s: bool; 
  mutable d: bool; 
  mutable q: bool; 
  mutable i: bool; 
  mutable k: bool;
  mutable j: bool; 
  mutable l: bool; 
  mutable u: bool; 

};;
let important_key_states = {w=false;a=false;s=false;d=false;q=false;i=false;k=false;j=false;l=false;u=false};;
let handle_event e = 
  match e with 
  | Event.Quit _ -> exit 0
  | Event.KeyDown e' -> begin
    match e'.Event.keycode with 
    | Keycode.A -> important_key_states.a<-true 
    | Keycode.W -> important_key_states.w<-true 
    | Keycode.S -> important_key_states.s<-true 
    | Keycode.D -> important_key_states.d<-true
    | Keycode.Q -> important_key_states.q<-true
    | Keycode.I -> important_key_states.i<-true
    | Keycode.K -> important_key_states.k<-true 
    | Keycode.J -> important_key_states.j<-true
    | Keycode.L -> important_key_states.l<-true
    | Keycode.U -> important_key_states.u<-true
    | _ -> ()
  end 
  | Event.KeyUp e' -> begin
    match e'.Event.keycode with 
    | Sdlkeycode.A -> important_key_states.a<-false;
    | Sdlkeycode.W -> important_key_states.w<-false;
    | Sdlkeycode.S -> important_key_states.s<-false;
    | Sdlkeycode.D -> important_key_states.d<-false;
    | Sdlkeycode.Q -> important_key_states.q<-false;
    | Sdlkeycode.I -> important_key_states.i<-false;
    | Sdlkeycode.K -> important_key_states.k<-false;
    | Sdlkeycode.J -> important_key_states.j<-false;
    | Sdlkeycode.L -> important_key_states.l<-false;
    | Sdlkeycode.U -> important_key_states.u<-false;
    | _ -> ()
  end
  | _ -> ()
;;
let handle_events elist = List.iter handle_event elist;;


let handle_game_state () =
  let p1_velx=ref 0. in
  let p1_vely=ref 0. in
  if important_key_states.a then p1_velx:=!p1_velx -. 1.;
  if important_key_states.d then p1_velx:=!p1_velx +. 1.;
  if important_key_states.w then p1_vely:=!p1_vely -. 1.;
  if important_key_states.s then p1_vely:=!p1_vely +. 1.;
  sherrif1#move !p1_velx !p1_vely;

  let p2_velx=ref 0. in
  let p2_vely=ref 0. in
  if important_key_states.j then p2_velx:=!p2_velx -. 1.;
  if important_key_states.l then p2_velx:=!p2_velx +. 1.;
  if important_key_states.i then p2_vely:=!p2_vely -. 1.;
  if important_key_states.k then p2_vely:=!p2_vely +. 1.;
  sherrif2#move !p2_velx !p2_vely;
  if important_key_states.q then sherrif1#aim() else sherrif1#shoot_if_aiming();
  if important_key_states.u then sherrif2#aim() else sherrif2#shoot_if_aiming();
  (sherrif1#get_bullet())#move();
  (sherrif2#get_bullet())#move();
;;

let rec mainloop () =
  if (!ticks > !world_last_deactivation+1000) then begin
    let elist=get_event_list () in 
    handle_events elist;
    handle_game_state ();
    draw ();
  end;
  Timer.delay ~ms:10;
  ticks := Timer.get_ticks ();
  if ((sherrif1#gethits()<10 && sherrif2#gethits()<10 ) || !ticks < !world_last_deactivation+1000) then mainloop ();
;;
let main () = 
  ticks := Timer.get_ticks ();
  mainloop ();
;;
main () ;;