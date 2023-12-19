library(advent)

input <- get_puzzle_input(2023, 19, stop = "EOS")
input <- input[1:length(input)]

test_input <- c(
  "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"
) |> strsplit("\n") |> unlist()

# input <- test_input

sep <- which(input == "")
workflows <- input[1:sep - 1]
parts <- input[(sep + 1):length(input)]

parse_workflow <- function(x) {
  splitted <- strsplit(gsub("}", "", x ), "{", fixed = T)
  w_name <- lapply(splitted, \(x) x[1])
  w_work <- sapply(lapply(splitted, \(x) x[2]), \(x) strsplit(x, ","),
                   simplify = T, USE.NAMES = F)
  names(w_work) <- w_name
  print(w_work)
}

parse_part <- function(x) {
  y <- gsub("[{}]", "", x) |>
    strsplit(",") |> unlist() |>
    sapply(\(x) (strsplit(x, "=")), USE.NAMES = F)
  y_out <- lapply(y, \(x) as.numeric(x[2]))
  names(y_out) <- lapply(y, \(x) x[1])
  return(y_out)
}

workflows_parsed <- sapply(workflows, parse_workflow,
                           simplify = T, USE.NAMES = F)
parts_parsed <- sapply(parts, parse_part,
                       simplify = F, USE.NAMES = F)

process_part <- function(p_l, w_name) {
  # Recursive: function calls itself until we reach a A or R

  # First, select the workflow among the workflow list
  w <- workflows_parsed[[w_name]]

  # Cycle the steps ouf the workflow, stopping early if needed
  i <- 1 ; w_finished <- FALSE
  while (!w_finished) {

    w_i <- w[i]

    if (w_i == "A") {

      return(TRUE)

    } else if (w_i == "R") {

      return(FALSE)

    } else if (grepl(":", w_i, fixed = T)) {

      # This is a logical test that needs to be evaluated
      splitted <- strsplit(w_i, ":", fixed = TRUE) |> unlist()
      log_test <- splitted[1]
      fall_back <- splitted[2]
      test_result <- eval(parse(text = log_test), envir = p_l)

      if (test_result) {

        if (fall_back == "A") {

          return(TRUE)

        } else if (fall_back == "R") {

          return(FALSE)

        } else {

          ret <- process_part(p_l, fall_back)
          w_finished <- TRUE

        }

      } else {

        i <- i + 1

      }

    } else {

      ret <- process_part(p_l, w_i)
      w_finished <- TRUE

    }

  }

  return(ret)
}

process_part(p_l = parts_parsed[[200]], w_name = "in")

accepted <- sapply(parts_parsed[1:200], process_part, w_name = "in")

parts_parsed[accepted] |> unlist() |> sum()
