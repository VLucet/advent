library(advent)

test_input <-
  c("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

input <- readLines("inputs/2023/day_15.txt")

parsed <- sapply(test_input, \(x) strsplit(x, ",") |> unlist(),
                 USE.NAMES = F, simplify = F)  |> unlist()

to_asci <- \(x) strtoi(charToRaw(x), 16L)

parsed_num <- sapply(parsed, \(x) sapply(unlist(strsplit(x, "")),
                                         \(x) to_asci(x)),
                     simplify = F, USE.NAMES = F)
parsed_num

hash_pass <- \(x) (x * 17) %% 256

hash_alg <- function(x){
  ret  <- hash_pass(x[1])
  for (y in x[2:length(x)]) {
    ret <- hash_pass(ret + y)
  }
  ret
}

sapply(parsed_num, hash_alg) |> sum()

# Part 2

parsed_split <- sapply(parsed, strsplit, "[=-]",
                       simplify = T, USE.NAMES = F)

box_list <- list()

for (i in 1:length(parsed_split)) {
  inst <- parsed_split[[i]]
  inst_num <- parsed_num[[i]]

  box_id <- as.character(hash_alg(
    sapply(unlist(strsplit(inst[1], "")), to_asci)
  ))

  the_box <- box_list[[box_id]]
  if (length(inst) == 1) {
    if (inst %in% names(the_box)) {
      the_box[[inst]] <- NULL
    }
  } else {
    if (inst[1] %in% names(the_box)) {
      the_box[[inst]] <- inst[2]
    } else {
      the_box[[inst[1]]] <- inst[2]
    }
  }
  box_list[[box_id]] <- the_box
}

box_list <- Filter(\(x) length(x) > 0, box_list)

sapply(1:length(box_list), function(x) {
  # browser()
  the_box <- box_list[x]
  base_mult <- as.numeric(names(the_box))
  slots <- 1:length(the_box[[1]])
  slots * as.numeric(unlist(the_box[[1]]))

})
