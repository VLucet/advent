
library(advent)

input <- get_puzzle_input(2023,2)

stopifnot(length(input) == 100)

test_input <- c(
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
  "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
  "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
  "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
  "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
)

# -------------------------------------------------------------------------

parse_line <- function(the_line){
  splitted <- unlist(strsplit(x = the_line, split = ": ", fixed = T))
  id <- as.numeric(gsub("Game ", "", splitted[1]))
  games <- unlist(strsplit(splitted[2], "; "))

  games_splitted <- sapply(games, (\(x) strsplit(x, ", ", fixed = T)),
                           simplify = T, USE.NAMES = F)

  games_parsed <- lapply(games_splitted, \(x) {

    sapply(x, \(y) {

      inner_list <- list()
      y_split <- unlist(strsplit(y, " "))
      inner_list[[y_split[2]]] <- as.numeric(y_split[1])
      inner_list

    }, USE.NAMES = F)

  })

  return(list(id = id,
              sets = games_parsed))

}

summarize_game <- function(parsed_game){
  data.frame(id = parsed_game$id,
             blue_sum = sum(sapply(parsed_game$sets, \(x) ifelse(is.null(x$blue), 0, x$blue))),
             red_sum = sum(sapply(parsed_game$sets, \(x) ifelse(is.null(x$red), 0, x$red))),
             green_sum = sum(sapply(parsed_game$sets, \(x) ifelse(is.null(x$green), 0, x$green))),
             blue_max = max(sapply(parsed_game$sets, \(x) ifelse(is.null(x$blue), 0, x$blue))),
             red_max = max(sapply(parsed_game$sets, \(x) ifelse(is.null(x$red), 0, x$red))),
             green_max = max(sapply(parsed_game$sets, \(x) ifelse(is.null(x$green), 0, x$green))))
}

possible_ids <- function(parsed_table, blue = 14, red = 12, green = 13){
  parsed_table[parsed_table$blue_max <= blue &
                 parsed_table$red_max <= red &
                 parsed_table$green_max <= green,
               "id"]
}

# -------------------------------------------------------------------------

# parsed_games <- sapply(test_input, parse_line,
#                        USE.NAMES = F, simplify = F)

parsed_games <- sapply(input, parse_line, USE.NAMES = F, simplify = F)
game_summaries <-  do.call(rbind, sapply(parsed_games, summarize_game,
                                         USE.NAMES = F, simplify = F))
# Part 1
possible <- possible_ids(game_summaries)
sum(possible) == 2176

# Part 2
sum(game_summaries$blue_max * game_summaries$red_max * game_summaries$green_max) == 63700
