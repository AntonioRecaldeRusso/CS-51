open Core.Std
(** A helper module for drawing graphics. *)

(** Draws a circle with lower left corner at position (x,y) with the specified
    width and height.  The circle is drawn with color bg and the text t is
    drawn with color fg on top of the circle. *)
let circle ((x,y):int*int) (width:int) (height:int)
           (bg:Graphics.color) (fg:Graphics.color) (t:string) : unit =
  Graphics.set_color bg ;
  Graphics.fill_circle (x*width + width/2) (y*height + height/2)
                       (min width height / 2) ;
  Graphics.moveto (x*width+2) (y*height) ;
  Graphics.set_color fg ;
  Graphics.draw_string t

(** Draw a status bar for an object positioned at (x,y) and with the specified
    width and height. Status bars are horizontal bars drawn at the top of an
    objects drawing cell. The bar will be drawn with the specified bar_height.
    The width of the bar will be full for v=1.0 and empty for v=0.0. *)
let status_bar ((x,y):int*int) (width:int) (height:int)
               (c:Graphics.color) (bar_height:int) (v:float) : unit =
  Graphics.set_color c ;
  let bar_width = Float.to_int (v *. Float.of_int width) in
  Graphics.fill_rect (x*width) (y*height + height - bar_height)
    bar_width bar_height
