library(advent)
library(DescTools)

input <- get_puzzle_input(2023,8,stop = "EOS")

test_input <- c(
  "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"
) |> strsplit("\n") |> unlist()

test_input_2 <- c(
  "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"
) |> strsplit("\n") |> unlist()

# -------------------------------------------------------------------------

# Part 1

# insts <- test_input[1]
# lines <- test_input[3:length(test_input)]

insts <- input[1]
lines <- input[3:length(input)]

insts_parsed <- gsub(insts, pattern = "L", replacement = "1") |>
  gsub(insts, pattern = "R", replacement = "2") |>
  strsplit("") |> unlist() |> as.numeric()

lines_separated <- sapply(lines, strsplit, " = ", fixed = T)
lines_parsed <- lines_separated |>
  sapply(\(x) strsplit(gsub(x[2], pattern = "([()])", replacement = ""),
                       ", ", fixed = T))
names(lines_parsed) <- lines_separated |>
  lapply(\(x) x[1])
lines_parsed

walk_network <- function(insts = insts_parsed, map = lines_parsed,
                         start = "AAA", end = "ZZZ") {
  node <- start
  step <- 1
  # print(node)
  while (node != end) {
    dir <- step %% length(insts)
    dir <- ifelse(dir == 0, length(insts), dir)
    node <- map[[node]][insts[dir]]
    # print(node)
    step <- step + 1
  }
  return(step - 1)
}

walk_network(insts_parsed, lines_parsed)


# -------------------------------------------------------------------------

# Part 2

test_input_3 <- c(
  "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
") |> strsplit("\n") |> unlist()

# insts <- test_input_3[1]
# lines <- test_input_3[3:length(test_input_3)]

insts <- input[1]
lines <- input[3:length(input)]

insts_parsed <- gsub(insts, pattern = "L", replacement = "1") |>
  gsub(insts, pattern = "R", replacement = "2") |>
  strsplit("") |> unlist() |> as.numeric()

lines_separated <- sapply(lines, strsplit, " = ", fixed = T)
lines_parsed <- lines_separated |>
  sapply(\(x) strsplit(gsub(x[2], pattern = "([()])", replacement = ""),
                       ", ", fixed = T))
names(lines_parsed) <- lines_separated |>
  lapply(\(x) x[1])
lines_parsed

starting_points_id <- which( (lapply(strsplit(names(lines_parsed), ""),
                                     \(x) x[3]) |> unlist()) == "A" )

same_ends_in_Z <- function(node, starting_node){
  unlist(strsplit(node, ""))[3] == "Z" # &
    # all(unlist(strsplit(node, ""))[1:2] == unlist(strsplit(starting_node, ""))[1:2])
}

walk_network_2 <- function(insts, map, start_ids = starting_points_id) {

  # browser()
  starting_nodes <- names(map)[start_ids][names(map)[start_ids] != "AAA"]

  nodes <- starting_nodes

  # step_list <- c()

  step <- 1

  while (!all(mapply(nodes, starting_nodes,
                     FUN = same_ends_in_Z))) {

    dir <- step %% length(insts)
    dir <- ifelse(dir == 0, length(insts), dir)

    # node <- map[[node]][insts[dir]]
    nodes <- sapply(nodes, \(x) map[[x]][insts[dir]])

    if (step %% 1000000 == 0) print(unname(nodes))

    step <- step + 1

    # if (any(mapply(nodes, starting_nodes,
    #                FUN = same_ends_in_Z))) {
    #   step_list <- c(step_list, step)
    #   print(step_list)
    #   if (length(step_list) == length(starting_nodes)) return(step_list)
    # }
  }
  return(step - 1)
}

x <- walk_network_2(insts_parsed, lines_parsed)

starts <- names(lines_parsed)[starting_points_id]
ends <- sapply(starts, function(x) {
  splitted <- strsplit(x, "") |> unlist()
  splitted[3] <- "Z"
  paste0(splitted, collapse = "")
}, simplify = F) |> unlist() |> unname()

mapply(FUN = walk_network, start = starts[1], end = ends[1])
