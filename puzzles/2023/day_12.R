library(advent)

# input <- get_puzzle_input(2023,12)

# stopifnot(length(input) == 1000)

test_input <- c(
  "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"
) |> strsplit("\n") |> unlist()

# -------------------------------------------------------------------------

input <- test_input

# -------------------------------------------------------------------------

parse <- function(the_line) {
  # browser()
  splitted <- strsplit(the_line, " ") |> unlist()
  splitted_sign <- strsplit(splitted[1], "") |> unlist()
  splitted_groups <- strsplit(splitted[2], ",") |> unlist() |> as.numeric()
  list(sign = splitted[1],
       g_broken = splitted_groups,
       g_remain = length(splitted_sign) - sum(splitted_groups),
       g_empty = length(splitted_groups) + 1)
}

parsed <- sapply(input, parse, USE.NAMES = F, simplify = F)
str(parsed[[1]])

compute_combinations <- function(line_dat) {
  # browser()
  combs <- t(combn(line_dat$g_remain + line_dat$g_empty - 1,
                   line_dat$g_remain) - c((1:line_dat$g_remain) - 1))
  middle_sections <- 2:(line_dat$g_empty - 1)
  rules <- apply(combs, 1, \(x) all(sapply(middle_sections, \(y) any(x %in% y))))
  line_dat$combs <- combs[rules, ]
  line_dat
}

parsed_with_combs <- lapply(parsed, compute_combinations)
str(parsed_with_combs[[2]])

generate_regex <- function(template) {
  # browser()
  pattern <- paste0("^", paste(sapply(unlist(strsplit(template, "")),
                                      function(char) {
                                        if (char == "?") {
                                          return(".")
                                        } else {
                                          return(paste0("[", char, "]"))
                                        }
                                      }), collapse = ""), "$")

  return(pattern)
}

build_strings <- function(line_dat) {

  # browser()

  combs <- line_dat$combs
  if (is.null(dim(combs)[1])) {
    combs <- matrix(combs, ncol = length(combs))
  }

  empties <- t(apply(combs, 1,
                     \(x) sapply(1:line_dat$g_empty,
                                 \(y) sum(x == y))))

  broken_str <- sapply(line_dat$g_broken, \(x) paste0(rep("#", x), collapse = ""))
  broken_str <- c(broken_str, "")
  spacers_str <- apply(empties, 1, \(x) sapply(x, \(y) paste0(rep(".", y), collapse = ""),
                                               simplify = T), simplify = F)

  strs <- sapply(spacers_str, \(x) paste0(paste0(x, broken_str), collapse = ""),
                 simplify = F, USE.NAMES = F) |>
    unlist()

  strs <- strs[grep(generate_regex(line_dat$sign), strs)]

  line_dat$strings <- strs
  line_dat
}

parsed_with_strs <- lapply(parsed_with_combs, build_strings)
str(parsed_with_strs[[1]])

lapply(parsed_with_strs, \(x) x[["strings"]]) |> unlist() |> length()

# -------------------------------------------------------------------------

library(parallel)

n.cores <- detectCores()
n.cores

parse_2 <- function(the_line) {
  # browser()
  splitted <- strsplit(the_line, " ") |> unlist()

  splitted[1] <- paste0(rep(splitted[1], 5), collapse = "?")
  splitted[2] <- paste0(rep(splitted[2], 5), collapse = ",")

  splitted_sign <- strsplit(splitted[1], "") |> unlist()
  splitted_groups <- strsplit(splitted[2], ",") |> unlist() |> as.numeric()
  list(sign = splitted[1],
       g_broken = splitted_groups,
       g_remain = length(splitted_sign) - sum(splitted_groups),
       g_empty = length(splitted_groups) + 1)
}

parsed <- sapply(input, parse_2, USE.NAMES = F, simplify = F)
str(parsed[[1]])

clust <- makeCluster(n.cores - 2)
clusterExport(clust, "parsed")

parsed_with_combs <- parLapply(clust, parsed, fun = compute_combinations)
str(parsed_with_combs[[2]])

parsed_with_strs <- lapply(parsed_with_combs, build_strings)
str(parsed_with_strs[[1]])

lapply(parsed_with_strs, \(x) x[["strings"]]) |> unlist() |> length()
