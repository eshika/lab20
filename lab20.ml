type image = float list list ;;
(* images are lists of lists of floats between 0. (white) and 1. (black) *)
open Graphics ;;
open List ;;

(* threshold thershold image -- image where pixels above the threshold
value are black *)
let threshold img threshold =
  map  (fun row ->
              map (fun v -> if v <= threshold then 0. else 1.)
                                 row) img

(* show the image *)
let depict img  =
  open_graph "";
  clear_graph ();
  let x, y = length (hd img), length img in
            resize_window x y;
  let depict_pix v r c =
      let lvl = int_of_float (255. *. (1. -. v)) in
                        set_color (rgb lvl lvl lvl);
  plot c (y - r) in
  iteri (fun r row ->
                iteri (fun c pix -> depict_pix pix r c) row) img;
  Unix.sleep 2;
  close_graph () ;;

(* dither max image -- dithered image *)
let dither img =
  map (fun row ->
            map (fun v -> if v > Random.float 1. then 1. else 0.)
                                row) img ;;

depict Monalisa.image ;;
depict (threshold Monalisa.image 0.75) ;;
depict (dither Monalisa.image) ;;

