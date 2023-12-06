library(advent)

input <- get_puzzle_input(2023,5,stop = "EOS")

test_input <- c(
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"
) |> strsplit("\n") |> unlist()

# Part 1
# input_no_ret <- test_input[test_input != ""]
input_no_ret <- input[input != ""]

seeds_pos <- which(grepl("seeds:", input_no_ret, fixed = T))
seeds <- gsub(pattern = "seeds: ", replacement = "", input_no_ret[seeds_pos]) |>
  strsplit(" ") |> unlist() |> as.numeric()

maps_pos <- which(grepl("map:", input_no_ret, fixed = T))
maps_fromto <-
  sapply(maps_pos, \(x) input_no_ret[x] |>
           gsub(pattern = " map:", replacement = ""))
maps_fromto_vec <- maps_fromto |>
  strsplit("-to-")

maps_mats <-
  mapply(maps_pos, c(maps_pos[2:length(maps_pos)], length(input_no_ret) + 1),
         FUN = function(x, y){
           maps_vec <- input_no_ret[(x + 1):(y - 1)]
           maps_vec_num <-
             sapply(maps_vec, \(x) as.numeric(unlist(strsplit(x, " "))),
                    USE.NAMES = F)
           return(maps_vec_num)
         })

maps_mats_ranges <- mapply(maps_mats, maps_fromto_vec,
                           FUN = function(maps_mat, maps_fromto){
                             reaches <- apply(maps_mat, 2, FUN = \(x) x[3])
                             map_mat_cols <- apply(maps_mat, 2, FUN = \(x) x[1:2], simplify = F)

                             maps_extended <- mapply(FUN = function(col, reach, fromto){
                               ranges <- sapply(col, \(x) c(x, x + reach - 1), simplify = F)
                               # names(ranges) <- c(fromto[2], fromto[1])
                               names(ranges) <- c("to", "from")
                               ranges
                             }, map_mat_cols, reaches, MoreArgs = list(fromto = maps_fromto), SIMPLIFY = F)
                             names(maps_extended) <- paste0("set_", 1:length(maps_extended))

                             return(maps_extended)
                           })

names(maps_mats_ranges) <- maps_fromto

# -------------------------------------------------------------------------

# Manual search
search_maps <- function(seed) {
  print(paste0("Seed ", seed))
  value <- seed
  for (map_id in 1:length(maps_mats_ranges)) {
    map <- maps_mats_ranges[map_id][[1]]
    value_matched <- FALSE
    for (set in map) {
      if (!value_matched & (value >= set$from[1]) & (value <= set$from[2])) {
        print(names(maps_mats_ranges)[map_id])
        (str(set))
        value <- set$to[1] + (value - set$from[1])
        value_matched <- TRUE
      }
    }
    print(value)
  }
  value
}

locs <- unlist(lapply(seeds, search_maps))
locs
min(locs)

# -------------------------------------------------------------------------

# Part 2
seed_ranges <- matrix(c(seeds[which((1:length(seeds) %% 2) == T)],
                        seeds[which((1:length(seeds) %% 2) == T)] +
                          seeds[which((1:length(seeds) %% 2) != T)] - 1), ncol = 2)
range_list <- lapply(1:nrow(seed_ranges), \(x) seed_ranges[x,])


find_overlap <- function(x, y) {

  # browser()

  if (y[1] > x[2] | x[1] > y[2]) {
    ret <- list(inside = NULL,
                outside = x)
  } else {
    inter <- c(max(x[1], y[1]),
               min(x[2], y[2]))
    if (all(x == inter)) {
      ret <- list(inside = inter,
                  outside = NULL)
    } else {
      if (y[1] <= x[1]) {
        ret <- list(inside = inter,
                    outside = c(y[2] + 1, x[2]))
      } else if (y[2] >= x[2]) {
        ret <- list(inside = inter,
                    outside = c(x[1], y[1] - 1))
      } else {
        ret <- list(inside = inter,
                    outside = list(c(x[1], y[1] - 1),
                                   c(y[2] + 1, x[2])))
      }
    }

  }

  # browser()

  unlisted <- unlist(unlist(ret))
  stopifnot(min(unlisted) == x[1])
  stopifnot(max(unlisted) == x[2])

  sorted <- sort(unlisted)
  diffed <- diff(sorted)

  if (length(sorted) > 2) {
    print(diffed)
    middle <- diffed[2:(length(diffed) - 1)]
    if (!all(middle == 1)) {
      stopifnot(sum(middle != 1) == 1)
    }
  }

  # browser()

  return(ret)
}

my_fun <- function(){

  browser()

  all_ranges <- list()

  for (seed_range in range_list) { # range_list[1]) {

    # print(seed_range)
    valid_ranges <- list(seed_range)

    for (map_id in 1:length(maps_mats_ranges)) {

      # **
      print(names(maps_mats_ranges)[map_id])
      map <- maps_mats_ranges[map_id][[1]]
      # **

      status_list <- list(to_map = valid_ranges,
                          mapped = list())
      # str(status_list)

      if (length(status_list$to_map) != 0) {

        # for (to_map_id in 1:length(status_list$to_map)) {
        while (length(status_list$to_map) > 0) {

          for (set in map) {
          if (length(status_list$to_map) != 0) {
            # set <- map[[1]]
            # str(set)
            # browser()

            current_range <- status_list$to_map[[1]]

            overlap <- find_overlap(current_range, y = set$from)
            # print(overlap)

            if (is.null(overlap$inside)) {

              print("No overlap")

            } else {

              # mapping
              current_range_mapped <- c(
                set$to[1] + abs((overlap$inside[1] - set$from[1])),
                set$to[1] + abs((overlap$inside[2] - set$from[1])))

              status_list$mapped <- append(status_list$mapped, list(current_range_mapped))
              status_list$to_map[[1]] <- NULL

              if (!is.null(overlap$outside)) { # we append to to-map list what remains to be mapped
                if (is.list(overlap$outside)) {
                  for (overlap_range in overlap$outside) {
                    status_list$to_map <- append(status_list$to_map, list(overlap_range))
                  }
                } else {
                  status_list$to_map <- append(status_list$to_map, list(overlap$outside))
                }
              }
            }

            # if (length(status_list$to_map) == 0) {
            #   browser()
            # }
          }
          }

          if (length(status_list$to_map) > 0) {
            status_list$mapped <- append(status_list$mapped, list(current_range_mapped))
            status_list$to_map[[1]] <- NULL
          }

        }

        if (length(status_list$to_map) > 0) {
          if (is.list(status_list$to_map)) {
            for (remaining_range in status_list$to_map) {
              status_list$mapped <- append(status_list$mapped, list(remaining_range))
            }
          } else {
            status_list$mapped <- append(status_list$mapped, list(status_list$to_map))
          }

          status_list$to_map <- list()
        }


      }
      valid_ranges <- status_list$mapped
    }

    all_ranges <- append(all_ranges, valid_ranges)
  }

  return(all_ranges)

}

out <- my_fun() ; sapply(out, \(x) unlist(x) |> min()) |> min()
