library(advent)

# input <- get_puzzle_input(2023, 17)

test_input <- c(
  "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"
) |> strsplit("\n") |> unlist()

# test_input <- c(
# "24134
# 32154
# 32552"
# ) |> strsplit("\n") |> unlist()

parsed_mat <- sapply(test_input, \(x) strsplit(x, "") |> unlist() |> as.numeric(), 
                     USE.NAMES = F) |> t()

## Attempting to implement Dijkstra

# -------------------------------------------------------------------------

get_neighbor_pos_rook <- function(point, mat = parsed_mat){
  pos <- list(
    c(point[1] + 1, point[2]),
    c(point[1] - 1, point[2]),
    c(point[1]    , point[2] + 1),
    c(point[1]    , point[2] - 1)
  )
  pos[sapply(pos, \(x) (x[1] > 0) & x[1] <= nrow(mat)) &
        sapply(pos, \(x) (x[2] > 0) & x[2] <= ncol(mat))]
}

pos_to_name <- \(x) paste0(x, collapse = "_")
name_to_pos <- \(x) strsplit(x, "_") |> unlist() |> as.numeric()

# -------------------------------------------------------------------------

names_mat <- matrix(apply(expand.grid(1:nrow(parsed_mat), 1:ncol(parsed_mat)), 
                          MARGIN = 1, FUN = pos_to_name), 
                    nrow = nrow(parsed_mat), ncol= ncol(parsed_mat))

distances <- matrix(Inf, nrow = nrow(parsed_mat), 
                    ncol = ncol(parsed_mat))
distances[1,1] <- parsed_mat[1,1]

previous_vertex <- matrix("", nrow = nrow(parsed_mat), 
                          ncol = ncol(parsed_mat))
previous_vertex[1,1] <- pos_to_name(c(1,1))

unvisited <- names_mat
names(unvisited) <- c(names_mat)
visited <- c()

# -------------------------------------------------------------------------

pos <- c(1,1)

while(!(all(is.na(unvisited)))) {
  
  print(pos)
  pos_name <- pos_to_name(pos)
  
  # Get neighbors
  neighs <- get_neighbor_pos_rook(pos)
  neighs_names <- sapply(neighs, pos_to_name)
  
  # Only look at neibhors which have not been visited
  neighs_locs <- names_mat %in% neighs_names[!(neighs_names %in% visited)]
  
  # Compute distances and replace when we find a smaller one, 
  # Update the previous vertex as well if needed
  dist_vec <- parsed_mat[neighs_locs] + distances[pos[1], pos[2]]
  previous_vertex[neighs_locs] <- ifelse(dist_vec < distances[neighs_locs], 
                                         pos_name, previous_vertex[neighs_locs])
  distances[neighs_locs] <- ifelse(dist_vec < distances[neighs_locs], 
                                   dist_vec, distances[neighs_locs])
  # Add to the visited
  visited <- c(visited, pos_name)
  
  # Find the next value in the priority queue
  unvisited[names_mat %in% pos_name] <- NA
  valid_unvisited <- which(!is.na(unvisited))
  which_min_distances <- which.min(distances[valid_unvisited])
  next_valid_unvisited <- unvisited[valid_unvisited[which_min_distances]]
  
  pos <- name_to_pos(next_valid_unvisited)
  
}

path <- c()
pos <- c(nrow(parsed_mat), ncol(parsed_mat))
while (pos_to_name(pos) != "1_1") {
  node <-  previous_vertex[pos[1], pos[2]]
  path <- c(path, node)
  pos <- name_to_pos(node)
}

path_list <- sapply(path, name_to_pos, USE.NAMES = F, simplify = F)
path_mat <- matrix(0, nrow = nrow(parsed_mat), 
                   ncol = ncol(parsed_mat))
for (pos in path_list) path_mat[pos[1], pos[2]] <- 1 

image(t(apply(path_mat, MARGIN = 2, FUN = rev)), asp=1)
sum(parsed_mat[path_mat == 1])
      