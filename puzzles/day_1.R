
library(advent)

input <- get_puzzle_input(2023,1)

stopifnot(length(input) == 1000)

get_calibration_value <- function(the_line){

  # Get the numbers only, replacing what is not (^) digits by nothing
  numbers <- gsub("[^[:digit:]]", "", the_line)

  # Get the correct characters
  value <- ifelse(nchar(numbers) > 0 & nchar(numbers) < 2,
                  paste0(numbers, numbers),
                  paste0(substring(numbers, 1,1), substring(numbers, nchar(numbers))))

  # Convert to num
  values_num <- as.numeric(value)

  return(values_num)
}

cal_values <- sapply(input, FUN = get_calibration_value)

## Answer to first part
sum(cal_values)

number_words <- c("one", "two", "three", "four", "five",
                  "six", "seven", "eight", "nine")
replaces <- c("on1ne", "tw2wo", "thre3hree", "fou4our", "fiv5ive",
              "si6ix", "sev7even", "eigh8ight", "nin9ine")

get_matches <- function(the_line, number_words){

  matches <- sapply( # get the minimum for each
    lapply( # unlist
      # lapply the expression
      lapply(number_words, gregexpr, the_line),
      # lapply the expression
      unlist), # unlist
    min)

  matches[matches == -1] <- 0

  return(matches)
}

get_calibration_value_again <- function(the_line, number_words, replaces){

  print(the_line)

  # Substitute words with numbers
  matches <- get_matches(the_line, number_words)
  print(matches)

  while (sum(matches) > 0 ) {
    non_zero <- which(matches != 0)
    print(non_zero)
    non_zero_min <- which(matches == min(matches[non_zero]))
    print(non_zero_min)
    the_line <- sub(number_words[non_zero_min], replaces[non_zero_min], the_line)
    print(the_line)
    matches <- get_matches(the_line, number_words)
    print(matches)
  }

  # Get the numbers only, replacing what is not (^) digits by nothing
  numbers <- gsub("[^[:digit:]]", "", the_line)

  print(numbers)

  # Get the correct characters
  value <- ifelse(nchar(numbers) > 0 & nchar(numbers) < 2,
                  paste0(numbers, numbers),
                  paste0(substring(numbers, 1,1), substring(numbers, nchar(numbers))))

  # Convert to num
  values_num <- as.numeric(value)

  return(values_num)
}

testset <- c("two1nine",
             "eightwothree",
             "abcone2threexyz",
             "xtwone3four",
             "4nineeightseven2",
             "zoneight234",
             "7pqrstsixteen")
stopifnot(sum(sapply(testset, FUN = get_calibration_value_again, number_words, replaces)) == 281)

cal_values_again <- sapply(input, FUN = get_calibration_value_again, number_words, replaces)

## Answer to second part
sum(cal_values_again)
