library(advent)

input <- get_puzzle_input(2023, 16)

test_input <- c(
  ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|...."
) |> strsplit("\n") |> unlist()

parsed_mat <- sapply(input, \(x) strsplit(x, "") |> unlist(), 
                     USE.NAMES = F) |> t()

dir_map <- list(
  "top"    = c(+1, +0),
  "bottom" = c(-1, +0),
  "left"   = c(+0, +1),
  "right"  = c(+0, -1)
)

char_to_dir <- list(
  "."  = list(
    "top"    = "top"   ,
    "bottom" = "bottom",
    "left"   = "left"  ,
    "right"  = "right" 
  ),
  "\\" = list(
    "top"    = "left",
    "bottom" = "right",
    "left"   = "top",
    "right"  = "bottom"
  ),
  "/"  = list(
    "top"    = "right",
    "bottom" = "left",
    "left"   = "bottom",
    "right"  = "top"
  ),
  "-"  = list(
    "top"    = c("right", "left"),
    "bottom" = c("right", "left"),
    "left"   = "left",
    "right"  = "right"
  ),
  "|"  = list(
    "top"    = "top",
    "bottom" = "bottom",
    "left"   = c("bottom", "top"),
    "right"  = c("bottom", "top")
  )
)

# ----------------------------------------------------

is_in_bounds <- function(x, y, mat){
  ((x >= 1) & (x <= nrow(mat))) &
    ((y >= 1) & (y <= ncol(mat)))
}

cell_name <- function(x, y, dir) {
  paste0(c(x, y, dir),  collapse = "_")
}

energize <- function(x, y, dir, mat) {
  
  # mat_ener <- matrix(0, nrow = nrow(mat),
  #                    ncol = ncol(mat))
  
  good <- c()
  handled <- list()
  todo <- list()
  todo[[cell_name(x, y, dir)]] <- list(x=x, y=y, dir=dir)
  
  while ((!is.null(length(todo))) & (length(todo)>0)) {
    
    # browser()
    
    # Equivalent of popping
    doing <- todo[1]
    todo[1] <- NULL
    
    if (names(doing) %in% names(handled)) {
      next
    }
    
    handled[[names(doing)]] <- doing[[1]]

    x <- doing[[1]]$x
    y <- doing[[1]]$y
    dir <- doing[[1]]$dir
    
    if (!is_in_bounds(x, y, mat)) {
      next
    }
    
    good <- c(good, paste0(c(x, y), collapse = "_"))
    
    char <- mat[x,y]
    # mat_ener[x,y] <- 1
    # image(z=mat_ener, asp=1)
    
    char_dir <- char_to_dir[[ char ]][[ dir ]]
    
    for (dir_i in char_dir){
      
      dir_num <- dir_map[[dir_i]]
      
      x_new <- x + dir_num[1]
      y_new <- y + dir_num[2]
      
      todo[[cell_name(x_new, y_new, dir_i)]] <- 
        list(x=x_new, y=y_new, dir=dir_i)
      
    }
    
  }
  
  return(length(unique(good)))
}

x <- energize(1, 1, "left", parsed_mat)
sum(x)

all_entries <- expand.grid(1:nrow(parsed_mat), 1:ncol(parsed_mat))

combs <- 
  rbind(cbind(expand.grid(1:nrow(parsed_mat), 1), dir="left"),
        cbind(expand.grid(1:nrow(parsed_mat), ncol(parsed_mat)), dir="right"),
        cbind(expand.grid(1, 1:ncol(parsed_mat)), dir="top"),
        cbind(expand.grid(1:nrow(parsed_mat), 1), dir="bottom"))

x <- mapply(energize, combs[,1], combs[,2], combs[,3], list(parsed_mat),
            SIMPLIFY = F, USE.NAMES = F)
max(unlist(x))