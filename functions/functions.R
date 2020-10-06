traject_by_component <- function(initial_x, initial_y, initial_h = 0) {
   # Input:
   # initial_x: x-component of the initial launch velocity
   # initial_y: y-component of the initial launch velocity
   # initial_h: height of the initial launch
   #
   # Output:
   # list of the following elements:
   #
   # x0:   numeric scalar  - original initial_x input
   # y0:   numeric scalar  - original initial_x input
   # h0:   numeric scalar  - original initial_h input
   # form: function        - takes in single numeric and outputs corresponding height
   # H:    numeric vector  - max height achieved c(time, distance from launch, max height)
   # R:    numeric vector  - max distance achievet c(time, max distance, height on land)
   #
   
   stopifnot(
      is.numeric(initial_x) && is.numeric(initial_x) && is.numeric(initial_x)
   )
   
   # constant: acceleration due to gravity
   g <- 9.81 # m/s^2
   
   # formula that gives y(x) - to be plotted
   draw_trajectory <-  function(x) {
      # Function that takes in arbitrary number x, and returns the
      # corresponding height for the object's inputted trajectory.
      # (used for plots)
      
      # NOTE: We deliberately leave initial_x/y undefined in inner function args
      # so that R references the ones defined in traject_by_comp.
      (-g) / (2 * initial_x^2) * (x^2) + (initial_y / initial_x)*x + initial_h
   }
   
   # max height (y) reached
   max_height_reached <-
      c(
         initial_y / g,
         initial_x * initial_y / g,
         ((initial_y^2) / (2*g)) + initial_h
      ) %>%
      setNames(
         c(
            "t",
            "x",
            "y"
         )
      )
   
   # max range (x) reached
   max_range_reached <-
      c(
         (initial_y + sqrt(initial_y^2 + 2*g*initial_h)) / g,
         initial_x * (initial_y + sqrt((initial_y^2) + (2 * g *initial_h))) / g,
         0
      ) %>%
      setNames(
         c(
            "t",
            "x",
            "y"
         )
      )
   
   return(
      list(
         x0    = initial_x,
         y0    = initial_y,
         h0    = initial_h,
         form  = draw_trajectory,
         H     = max_height_reached,
         R     = max_range_reached
      )
   )
}


