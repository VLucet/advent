library(advent)

input <- get_puzzle_input(2023,9)

test_input <- c(
"0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"
) |> strsplit("\n") |> unlist()


# -------------------------------------------------------------------------

# Part 1

parsed <-
  sapply(input, simplify = F, USE.NAMES = F,
         \(x) sapply(unlist(strsplit(x, " ")), as.numeric, simplify = T, USE.NAMES = F))
head(parsed)

derive_to_0 <- function(x) {
  deriv_list <- list()
  while (!all(x == 0)) {
    x <- diff(x)
    deriv_list <- append(deriv_list, list(x))
  }
  return(deriv_list)
}

parsed_derived <- sapply(parsed, derive_to_0)
head(parsed_derived)

mapply(parsed_derived, parsed,
      FUN = \(x, y) rev(y)[1] + rev(cumsum(sapply(x, \(x) rev(x)[1])))[1]) |>
  sum()

# Part 2

parsed_2 <- sapply(parsed, rev, simplify = F, USE.NAMES = F)

parsed_derived_2 <- sapply(parsed_2, derive_to_0)

mapply(parsed_derived_2, parsed_2,
       FUN = \(x, y) rev(y)[1] + rev(cumsum(sapply(x, \(x) rev(x)[1])))[1]) |>
  sum()

