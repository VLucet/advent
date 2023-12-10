library(advent)

input <- get_puzzle_input(2023,10)

test_input <- c(
  "-L|F7
7S-7|
L|7||
-L-J|
L|-JF"
)

test_input_2 <- c(
  "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ"
)

test_input_3 <- c(
  "OF----7F7F7F7F-7OOOO
O|F--7||||||||FJOOOO
O||OFJ||||||||L7OOOO
FJL7L7LJLJ||LJIL-7OO
L--JOL7IIILJS7F-7L7O
OOOOF-JIIF7FJ|L7L7L7
OOOOL7IF7||L7|IL7L7|
OOOOO|FJLJ|FJ|F7|OLJ
OOOOFJL-7O||O||||OOO
OOOOL---JOLJOLJLJOOO"
)

test_input_4 <- c(
  "..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
.........."
)

# Part 1

parse_input <- function(x) {
  sapply(sapply(x, strsplit, "\n", simplify = T, USE.NAMES = F),
         strsplit, "", simplify = T, USE.NAMES = F)
}

# parsed <- do.call(rbind, parse_input(test_input_3))
parsed <- do.call(rbind, parse_input(input))
parsed <- rbind(".", cbind(".", parsed, ".") , ".")
parsed

starting_pos <- which(parsed == "S", arr.ind = T)
starting_pos

get_neighbor_pos_rook <- function(point){
  list(
    c(point[1] + 1, point[2]),
    c(point[1] - 1, point[2]),
    c(point[1]    , point[2] + 1),
    c(point[1]    , point[2] - 1)
  )
}

transforms <-
  list("|" = list(c(+1,0), c(-1,0)),
       "-" = list(c(0,+1), c(0,-1)),
       "L" = list(c(0,+1), c(-1,0)),
       "J" = list(c(-1,0), c(0,-1)),
       "7" = list(c(0,-1), c(+1,0)),
       "F" = list(c(0,+1), c(+1,0)),
       "S" = get_neighbor_pos_rook(c(0,0)))

walk_step <- function(pos, previous_pos = NA) {
  pos_char <- parsed[pos[1], pos[2]]
  allowed_neighs <- transforms[[pos_char]]
  neighs_pos <- lapply(allowed_neighs, \(x) c(pos[1] + x[1],
                                              pos[2] + x[2]))
  neighs <- lapply(neighs_pos, \(x) parsed[x[1],x[2]])
  if (pos_char == "S") {
    neighs_transforms <- lapply(neighs, \(x) transforms[[x]])
    neighs_connections <- mapply(neighs_pos, neighs_transforms,
                                 FUN = function(n_pos, n_trans) {
                                   sapply(n_trans, \(x) n_pos + x, simplify = F)
                                 }, SIMPLIFY = F)
    matches <- sapply(neighs_connections,
                      \(x) any(unlist(sapply(x, \(x) all(x == pos), simplify = F))),
                      simplify = F) |> unlist()
    stopifnot(sum(matches) == 2)
    matches_id <- which(matches)
    next_cells_pos <- sapply(matches_id, \(x) neighs_pos[x])
    return(next_cells_pos)
  } else {
    # browser()
    next_cell_pos <- neighs_pos[sapply(neighs_pos, \(x)  !all(x == previous_pos))]
    return(next_cell_pos)
  }

}

temp_set <- walk_step(starting_pos)
previous_pos <- list(starting_pos)
steps <- 1

parsed_trace <- parsed
parsed_trace[parsed_trace != "S"] <- "."

pos_list_1 <- list(c(starting_pos), temp_set[[1]])
pos_list_2 <- list(temp_set[[2]])

while (any(temp_set[[1]] != temp_set[[2]])) {

  parsed_trace[temp_set[[1]][1], temp_set[[1]][2]] <- "0"
  parsed_trace[temp_set[[2]][1], temp_set[[2]][2]] <- "0"

  next_set <- mapply(FUN = walk_step,
                     temp_set, previous_pos, SIMPLIFY = F) |>
    unlist(recursive = F)

  pos_list_1 <- append(pos_list_1, list(next_set[[1]]))
  pos_list_2 <- append(pos_list_2, list(next_set[[2]]))

  parsed_trace[next_set[[1]][1], next_set[[1]][2]] <- "0"
  parsed_trace[next_set[[2]][1], next_set[[2]][2]] <- "0"

  steps <- steps + 1
  previous_pos <- temp_set
  temp_set <- next_set

  # print(next_set)
}

print(steps)

# Part 2
second_path_rev <- rev(pos_list_2)
second_path_rev[[1]] <- NULL

sequence <- unlist(list(pos_list_1, second_path_rev), recursive = F)
sequence <- append(sequence, sequence[1])

parsed_trace[parsed_trace == "S"] <- "0"
pipe_tot <- sum((parsed_trace == "0"))

stopifnot(length(sequence) == pipe_tot + 1)
prod_sum <- 0
for (i in 1:(length(sequence) - 1)) {
  pos <- sequence[[i]]
  next_pos <- sequence[[i + 1]]
  prod_sum <- prod_sum + ((pos[1] + next_pos[1]) * (pos[2] - next_pos[2]))
}
a <- abs(prod_sum)/2
a - 0.5 * pipe_tot + 1
