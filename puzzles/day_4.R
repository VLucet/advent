library(advent)

input <- get_puzzle_input(2023,4,cache = F)

stopifnot(length(input) == 209)

test_input <- c(
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
  "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
  "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
  "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
  "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
  "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
)

parse_line <- function(the_line){
  nocard <- unlist(strsplit(the_line, ": ", fixed = TRUE))[2]
  numbers <- unlist(strsplit(nocard, " | ", fixed = TRUE))
  numbers_split <- sapply(numbers, strsplit, split = " ", USE.NAMES = F)
  numbers_numeric <- sapply(numbers_split, as.numeric, USE.NAMES = F)
  numbers_numeric_no_na <- sapply(numbers_numeric, \(x) x[!is.na(x)],
                                  USE.NAMES = F)
  return(numbers_numeric_no_na)
}

# Part 1
parsed <- lapply(input, parse_line)
matches <- lapply(parsed, \(x) sum(x[[1]] %in% x[[2]])) |>
  unlist()
scores <- lapply(matches, \(x) round(2^(x - 1))) |>
  unlist()
sum(scores)

# Part 2
cards <- rep(1, length(input))
for (i in 1:length(cards)) {
  print(i)
  print(cards)
  reach_vec <- c(
    rep(0,i),
    rep(1,matches[i]),
    rep(0,length(cards) - matches[i] - i)
  )
  print(reach_vec)
  cards <- cards + (reach_vec * cards[i])
  print(cards)
}
sum(cards)
