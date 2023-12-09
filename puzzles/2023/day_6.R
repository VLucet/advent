library(advent)

input <- get_puzzle_input(2023,6)

# Part 1

test_input <- c(
"Time:      7  15   30
Distance:  9  40  200"
) |> strsplit("\n") |> unlist()

dat <- sapply(input, strsplit, "\\s+", simplify = T, USE.NAMES = F)

dat_num <- sapply(dat, \(x) x[2:length(x)] |> as.numeric(), simplify = F, USE.NAMES = F)
dat_df <- as.data.frame(dat_num, col.names = c("Time", "Distance"))

compute_solutions <- function(time, distance){
  opts <- 0:time
  dist_covered <- (time - opts) * opts
  # print(dist_covered)
  dist_diff <- dist_covered - distance
  n_sols <- sum(dist_diff > 0)
  return(n_sols)
}

dat_df$ret <- mapply(compute_solutions, dat_df$Time, dat_df$Distance)
prod(dat_df$ret)

# Part 2

dat <- sapply(input, strsplit, "\\s+", simplify = T, USE.NAMES = F)

dat_num <- sapply(dat, \(x) x[2:length(x)] |> as.numeric(), simplify = F, USE.NAMES = F)
dat_str <- sapply(dat_num, \(x) as.numeric(paste0(x, collapse = '')),
                  simplify = F, USE.NAMES = F)
dat_df <- as.data.frame(dat_str, col.names = c("Time", "Distance"))

dat_df$ret <- mapply(compute_solutions, dat_df$Time, dat_df$Distance)
dat_df$ret
