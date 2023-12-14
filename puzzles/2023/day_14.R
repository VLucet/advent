library(advent)

test_input <-
  c("O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....") |> strsplit("\n") |> unlist()

input <- get_puzzle_input(2023, 14)

parsed <- sapply(input, \(x) strsplit(x, ""), USE.NAMES = F)
parsed_mat <- do.call(rbind, parsed)

# -------------------------------------------------------------------------

parsed_mat_flip <- t(apply((parsed_mat), 2, rev))

shift_vec <- function(vec) {
  # browser()length(vec)
  loc <- length(vec)
  while (loc > 0) {
    if (vec[loc] == "O") {
      if (loc != length(vec)) {
        while (vec[loc + 1] == ".") {
          vec[loc] <- "."
          vec[loc + 1] <- "O"
          if (loc != length(vec) - 1) {
            loc <- loc + 1
          }
        }
      }
    }
    loc <- loc - 1
  }
  return(vec)
}

x <- do.call(rbind,
             apply(parsed_mat_flip, 1, FUN = shift_vec, simplify = F))
sum(colSums(x == "O") * (1:ncol(x)))

x <- c()
for (i in 1:1000) {
  for (dir in 1:4) {
    parsed_mat <-
      do.call(rbind, apply(t(apply((parsed_mat), 2, rev)),
                           1, FUN = shift_vec, simplify = F))
    # print(sum(rowSums(parsed_mat == "O") * (nrow(parsed_mat):1)))
    # x <- c(x, sum(rowSums(parsed_mat == "O") * (nrow(parsed_mat):1)))
  }
  x <- c(x, sum(rowSums(parsed_mat == "O") * (nrow(parsed_mat):1)))
  # x <- c(x, sum(colSums(parsed_mat == "O") * (1:ncol(parsed_mat))))
}
(1000000000 - 85) %% 93 ## period is 93
