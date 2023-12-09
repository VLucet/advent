
library(advent)

input <- get_puzzle_input(2023,3)

stopifnot(length(input) == 140)

test_input <- c(
  "467..114..",
  "...*......",
  "..35..633.",
  "......#...",
  "617*......",
  ".....+.58.",
  "..592.....",
  "......755.",
  "...$.*....",
  ".664.598.."
)

# Part 1
parse_line <- function(the_line){
  the_line |>
    gsub(pattern = ".", replacement = "z", fixed = T) |>
    gsub(pattern = "([[:punct:]])", replacement = "p") |>
    strsplit("") |> unlist()
}

parsed <- sapply(input, parse_line, USE.NAMES = F) |> t()

is_number <- t(parsed) %in% as.character(0:9)
sets <- which(is_number)
sets_groupings <- cumsum(c(1, abs(sets[-length(sets)] - sets[-1]) > 1))
number_pos <- split(sets, sets_groupings)
numbers <- sapply(number_pos, \(x) as.numeric(paste0(t(parsed)[x], collapse = "")))

parsed_padded <- rbind(NA, cbind(NA, parsed, NA) , NA)

# https://stackoverflow.com/questions/29105175/find-neighbouring-elements-of-a-matrix-in-r
ind = 2:(nrow(parsed_padded) - 1)
neigh = rbind(N  = as.vector(t(parsed_padded)[ind - 1, ind    ]),
              NE = as.vector(t(parsed_padded)[ind - 1, ind + 1]),
              E  = as.vector(t(parsed_padded)[ind    , ind + 1]),
              SE = as.vector(t(parsed_padded)[ind + 1, ind + 1]),
              S  = as.vector(t(parsed_padded)[ind + 1, ind    ]),
              SW = as.vector(t(parsed_padded)[ind + 1, ind - 1]),
              W  = as.vector(t(parsed_padded)[ind    , ind - 1]),
              NW = as.vector(t(parsed_padded)[ind - 1, ind - 1])) |> t()
has_punc_nei <- apply(neigh, 1, \(x) any(x  == "p", na.rm = T))

good_numbers <- sapply(number_pos, \(x) sum(has_punc_nei[x]) > 0)

sum(numbers[good_numbers])

# Part 2

parse_line <- function(the_line){
  the_line |>
    gsub(pattern = ".", replacement = "z", fixed = T) |>
    gsub(pattern = "*", replacement = "s", fixed = T) |>
    gsub(pattern = "([[:punct:]])", replacement = "_") |>
    gsub(pattern = "z", replacement = "_", fixed = T) |>
    gsub(pattern = "s", replacement = "*", fixed = T) |>
    strsplit("") |> unlist()
}

parsed <- sapply(input, parse_line, USE.NAMES = F) |> t()
parsed_padded <- rbind(NA, cbind(NA, parsed, NA) , NA)

is_number <- t(parsed_padded) %in% as.character(0:9)
sets <- which(is_number)
sets_groupings <- cumsum(c(1, abs(sets[-length(sets)] - sets[-1]) > 1))
number_pos <- split(sets, sets_groupings)
numbers <- sapply(number_pos, \(x) as.numeric(paste0(t(parsed_padded)[x], collapse = "")))

get_neibhors <- function(ind, mat = parsed_padded){
  ids <- c(ind - ncol(mat) - 1, ind - ncol(mat), ind - ncol(mat) + 1,
           ind - 1, ind + 1,
           ind + ncol(mat) - 1, ind + ncol(mat), ind + ncol(mat) + 1)
  neigh <- data.frame(
    x = ids,
    value = t(mat)[ids]
  )
  return(neigh)
}

x <- mapply(FUN = function(x, y) {
  # browser()
  tab <- do.call(rbind, (sapply(x, get_neibhors, simplify = F)))
  tab["num"] <- y
  tab <- tab[!is.na(tab$value),]
  tab <- unique(tab[tab$value == "*",])
  return(tab)
}, number_pos, as.list(numbers), SIMPLIFY = F)

star_table <- do.call(rbind, x)
sapply( split.data.frame(star_table, star_table$x) , function(x) {
  ifelse(nrow(x) > 1, prod(x$num), 0)
}) |> sum()
