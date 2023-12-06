#' Get puzzle input
#'
#' Get puzzle input for a given year and day
#'
#'@export

get_puzzle_input <- function(year, day, cache = TRUE,
                             base_url = "https://adventofcode.com",
                             stop = "") {

  url <- paste0(c(base_url, year, "day", day, "input"), collapse = "/")
  file_name <- paste0(c(year, "day", day, "input"), collapse = "_")
  cache_dir <- tempdir()

  if (cache) {
    cache_path <- file.path(cache_dir, file_name)
    if (file.exists(cache_path)) {
      input <- readLines(cache_path)
      return(input)
    }
  }

  browseURL(url)
  input <- parse_input(stop = stop)
  if (cache) {
    if (file.exists(cache_path)) {
      file.remove(cache_path)
    }
    writeLines(input, cache_path)
  }

  return(input)
}

# https://rdrr.io/github/deeplexR/lexicoR/man/cmd_input.html
parse_input <- function(stop = "") {
  msg <- NULL
  num <- 1
  message("Paste puzzle input below.")
  Sys.sleep(0.1)
  while (TRUE) {
    temp <- readline(paste0("line ", num, ": "))
    if (temp == stop) break
    num <- num + 1
    msg <- c(msg, temp)
  }
  return(msg)
}
