library(advent)

input <- get_puzzle_input(2023,11)

test_input <- c(
  "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."
) |> strsplit("\n") |> unlist()

# input <- test_input

# -------------------------------------------------------------------------

# Part_1

input_splitted <- sapply(input, strsplit, "", USE.NAMES = F)
input_mat <- do.call(rbind, input_splitted)

is_empty_rows <- apply(input_mat, 1, \(x) all(x == "."))
is_empty_col <- apply(input_mat, 2, \(x) all(x == "."))

encoded_mat <- matrix(1, nrow = nrow(input_mat), ncol = ncol(input_mat)) +
  (is_empty_rows)
encoded_mat <- t(t(encoded_mat) + is_empty_col)

galaxies_coords <- which(input_mat == "#", arr.ind = T)

dists <- sapply(1:(nrow(galaxies_coords) - 1), function(x) {

  # print("________________")

  # browser()
  dist_list <- list()

  for (i in (x + 1):nrow(galaxies_coords)) {

    gal_1 <- galaxies_coords[x,]
    gal_2 <- galaxies_coords[i,]

    # print(gal_2)

    gal_1_r <- gal_1[1]
    gal_1_c <- gal_1[2]

    gal_2_r <- gal_2[1]
    gal_2_c <- gal_2[2]

    dist_row <- gal_1_r - gal_2_r
    dist_col <- gal_1_c - gal_2_c

    # print(c(dist_row, dist_col))

    path_row <- ifelse(dist_row == 0, list(),
                       list(encoded_mat[gal_1_r:gal_2_r, gal_1_c])
    ) |> unlist()

    path_col <- ifelse(dist_col == 0, list(),
                       list(encoded_mat[gal_1_r, gal_1_c:gal_2_c])
    ) |> unlist()


    dist_list <- append(dist_list,
                        list(path_row[2:(length(path_row) - 0)],
                             path_col[2:(length(path_col) - 0)]))

  }

  # print("________________")

  return(dist_list)

}, simplify = F, USE.NAMES = F)

unlist(dists) |> sum()

# -------------------------------------------------------------------------

# Part_2

input_splitted <- sapply(input, strsplit, "", USE.NAMES = F)
input_mat <- do.call(rbind, input_splitted)

is_empty_rows <- apply(input_mat, 1, \(x) all(x == "."))
is_empty_col <- apply(input_mat, 2, \(x) all(x == "."))

encoded_mat <- matrix(1, nrow = nrow(input_mat), ncol = ncol(input_mat)) +
  (is_empty_rows * (1000000 - 1))
encoded_mat <- t(t(encoded_mat) + (is_empty_col * (1000000 - 1)))

galaxies_coords <- which(input_mat == "#", arr.ind = T)

dists <- sapply(1:(nrow(galaxies_coords) - 1), function(x) {

  # print("________________")

  # browser()
  dist_list <- list()

  for (i in (x + 1):nrow(galaxies_coords)) {

    gal_1 <- galaxies_coords[x,]
    gal_2 <- galaxies_coords[i,]

    # print(gal_2)

    gal_1_r <- gal_1[1]
    gal_1_c <- gal_1[2]

    gal_2_r <- gal_2[1]
    gal_2_c <- gal_2[2]

    dist_row <- gal_1_r - gal_2_r
    dist_col <- gal_1_c - gal_2_c

    # print(c(dist_row, dist_col))

    path_row <- ifelse(dist_row == 0, list(),
                       list(encoded_mat[gal_1_r:gal_2_r, gal_1_c])
    ) |> unlist()

    path_col <- ifelse(dist_col == 0, list(),
                       list(encoded_mat[gal_1_r, gal_1_c:gal_2_c])
    ) |> unlist()


    dist_list <- append(dist_list,
                        list(path_row[2:(length(path_row) - 0)],
                             path_col[2:(length(path_col) - 0)]))

  }

  # print("________________")

  return(dist_list)

}, simplify = F, USE.NAMES = F)

unlist(dists) |> sum()
