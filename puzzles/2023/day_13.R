library(advent)

input <- get_puzzle_input(2023, 13, stop = "EOS")
returns <- which(input == "")
chunks <- data.frame(x = c(1, returns[1:(length(returns) - 1)]),
                     y = c(returns))
parsed <- mapply(chunks$x, chunks$y, FUN = function(x, y) {
  splitted <- input[x:y][input[x:y] != ""] |>
    strsplit("")
  mat <- ifelse(do.call(rbind, splitted) == "#", 1, 0)
  return(mat)
})

test_input <- c(
  "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"
) |> strsplit("\n\n") |>
  unlist()

parsed <- sapply(test_input, function(x) {
  splitted <- strsplit(x, "\n") |> unlist()
  re_splitted <- sapply(splitted, strsplit, "",
                        simplify = T, USE.NAMES = F)
  mat <- ifelse(do.call(rbind, re_splitted) == "#", 1, 0)
  return(mat)
}, simplify = F, USE.NAMES = F)

# -------------------------------------------------------------------------

# mat <- parsed[[1]]
# sym_idx <- c(1, (1:ncol(mat))[which(!(1:ncol(mat) %% 2))])
# ranges <- apply(combn(sym_idx, 2), MARGIN = 2, \(x) sort(x[1]:x[2]))

compute_remainder <- function(mat) {

  # browser()

  ranges <- apply(combn(1:ncol(mat), 2), MARGIN = 2, \(x) sort(x[1]:x[2]))
  ranges_sym <- lapply(unique(ranges), function(x){
    if ((length(x) %% 2) == 0 & ((1 %in% x ) | ncol(mat) %in% x)) return(x)
  })
  ranges_sym <- Filter(Negate(is.null), ranges_sym)

  remainder <- sapply(ranges_sym, \(x) x[length(x)/2])

  sym_matches <- sapply(ranges_sym, function(range) {
    all(apply(mat[,range], MARGIN = 1, FUN = function(row) {
      all(row[1:(length(row)/2)] ==
            rev(row[((length(row)/2) + 1):length(row)]))
    }))
  })

  if ((sum(sym_matches) == 0) ) {
    return(0)
  } else {
    sym <- min(remainder[sym_matches])
  }

  # stopifnot(length(sym) == 1)
  return(sym)
}

mirrors <- lapply(parsed, function(mat) {
  c(compute_remainder(mat), compute_remainder(apply(t(mat), 2, rev)))
  # compute_remainder(mat) +
  #   compute_remainder(apply(t(mat), 2, rev)) * 100
})

mirrors_scores <- lapply(mirrors, \(x) x[1] + x[2]*100) |> unlist() |> sum()

# apply(combn(x, 2), MARGIN = 2, \(x) sort(x[1]:x[2]))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# mat <- parsed[[1]]
# sym_idx <- c(1, (1:ncol(mat))[which(!(1:ncol(mat) %% 2))])
# ranges <- apply(combn(sym_idx, 2), MARGIN = 2, \(x) sort(x[1]:x[2]))

compute_remainder_2 <- function(mat) {

  # browser()

  ranges <- apply(combn(1:ncol(mat), 2), MARGIN = 2, \(x) sort(x[1]:x[2]))
  ranges_sym <- lapply(unique(ranges), function(x){
    if ((length(x) %% 2) == 0 & ((1 %in% x ) | ncol(mat) %in% x)) return(x)
  })
  ranges_sym <- Filter(Negate(is.null), ranges_sym)

  remainder <- sapply(ranges_sym, \(x) x[length(x)/2])

  sym_matches <- sapply(ranges_sym, function(range) {
    # browser()
    x <- sum(apply(mat[,range], MARGIN = 1, FUN = function(row) {
      # browser()
      x <- sum(row[1:(length(row)/2)] !=
            rev(row[((length(row)/2) + 1):length(row)]))
      x
    }))
    x
  })

  only_one <- sym_matches == 1
  if (sum(only_one) > 0) {
    sym <- min(remainder[sym_matches == 1])
  } else {
    sym <- 0
  }

  # stopifnot(length(sym) == 1)
  return(sym)
}

mirrors_2 <- lapply(parsed, function(mat) {
  c(compute_remainder_2(mat), compute_remainder_2(apply(t(mat), 2, rev)))
})

mirrors_scores_2 <- lapply(mirrors_2, \(x) x[1] + x[2]*100) |> unlist() |> sum()
# apply(combn(x, 2), MARGIN = 2, \(x) sort(x[1]:x[2]))




