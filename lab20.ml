
open Graphics ;;

(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type image = float list list ;;

(* size of image in number of  columns (number of elements in list) and rows (number of lists) *)
type size = int * int ;;
      
(* show the image *)
let depict (img : image) : unit =
  Graphics.open_graph ""; 
  Graphics.clear_graph ();
  let x, y = List.length (List.hd img), List.length img in 
    Graphics.resize_window x y;
      let depict_pix v r c = 
        let lvl = int_of_float (255. *. (1. -. v)) in 
        Graphics.set_color (Graphics.rgb lvl lvl lvl);
        plot c (y - r) in
      vanessa-hu
        List.iteri (fun r row -> List.iteri (fun c pix -> depict_pix pix r c) row) img;
      
        for i = 1 to y do
          for j = 1 to x do
            depict_pix List.nth i img y x
          done
        done

      Unix.sleep 2; 
  Graphics.close_graph () ;;

(* threshold image -- image where pixels above the threshold
value are black *)
let threshold (img : image) (threshold : float) : image =
  List.map (fun row -> List.map (fun v -> if v <= threshold then 0. else 1.) row) img  

(* dither max image -- dithered image *)
let dither (img : image) : image =
  List.map (fun row -> List.map (fun v -> if v > Random.float 1. then 1. else 0.) row) img
    
let mona = Monalisa.image ;;
depict mona ;;
    
let mona_threshold = threshold mona 0.75 ;;
depict mona_threshold ;;
      
let mona_dither = dither mona ;;
depict mona_dither ;;
           
