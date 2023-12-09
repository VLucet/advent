library(advent)

input <- get_puzzle_input(2023,7)

test_input <- c(
"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"
) |> strsplit("\n") |> unlist()

# test_input <- c(
# "AAAAA 483
# AA8AA 220
# 23332 28
# TTT98 684
# 23432 765
# A23A4 34
# 23456 543
# "
# ) |> strsplit("\n") |> unlist()

# AAAAA => 1
# AA8AA => 2
# 23332 => 2
# TTT98 => 3
# 23432 => 3
# A23A4 => 4
# 23456 => 5

char_map <- list(
  "A" = 14,
  "K" = 13,
  "Q" = 12,
  "J" = 11,
  "T" = 10
)

char_map_J <- list(
  "A" = 14,
  "K" = 13,
  "Q" = 12,
  "J" = 1,
  "T" = 10
)

# -------------------------------------------------------------------------

# parsed <- strsplit(test_input, " ")
parsed <- strsplit(input, " ")
hands <- lapply(parsed, \(x) x[1]) |> unlist()
bids <- lapply(parsed, \(x) as.numeric(x[2])) |> unlist()

dat <- data.frame(hands = hands, bids = bids)
dat$hands_char <- sapply(dat$hands, strsplit, "")

# -------------------------------------------------------------------------

# Part 1

dat$hands_char_mapped <- sapply(dat$hands_char, function(x){
  sapply(x, function(y){
    el <- getElement(char_map, y)
    ifelse(is.null(el), as.numeric(y), el)
  })
}, simplify = F)

dat$div <- sapply(dat$hands_char, \(x) length(unique(x)))
dat$div_ord <- 6 - dat$div

dat$n_1s <- sapply(dat$hands_char, \(x) sum(table(x) == 1))
dat$n_2s <- sapply(dat$hands_char, \(x) sum(table(x) == 2))

dat$type_signat <- dat$div_ord*100 + dat$n_1s*10 + dat$n_2s*1

dat

get_mapped_char <- function(i, ...) {
  sapply(dat$hands_char_mapped, \(x) x[i])
}
char_mapped <- as.data.frame(sapply(1:5, get_mapped_char))
colnames(char_mapped) <- paste0("C", 1:5)
dat <- cbind(dat, char_mapped)

dat_ordered <- dat[order(dat$type_signat,
                         dat$C1,
                         dat$C2,
                         dat$C3,
                         dat$C4,
                         dat$C5,
                         decreasing = T),]
dat_ordered$rank <- nrow(dat_ordered):1
dat_ordered
sum(dat_ordered$bids * dat_ordered$rank) # 6440

# -------------------------------------------------------------------------

# Part 2

dat$hands_char_mapped <- sapply(dat$hands_char, function(x){
  sapply(x, function(y){
    el <- getElement(char_map_J, y)
    ifelse(is.null(el), as.numeric(y), el)
  })
}, simplify = F)

dat$tab_max <- sapply(dat$hands_char_mapped, function(x){
  tab_x <- table(x[x != 1])
  if (length(tab_x > 0)) {
    return(
      max(as.numeric(names(tab_x))[which(tab_x == max(tab_x, na.rm = T))], na.rm = T)
    )
  } else {
    return(14)
  }
})

dat$whereJ <- sapply(dat$hands_char_mapped, \(x) which(x == 1))

dat$hands_char_mapped_J <- mapply(dat$hands_char_mapped, dat$whereJ, dat$tab_max,
                                  FUN = function(char_mapped, where_j, t_max){
                                    char_mapped[where_j] <- t_max
                                    char_mapped
                                  }, SIMPLIFY = F)

dat$div <- sapply(dat$hands_char_mapped_J, \(x) length(unique(x)))
dat$div_ord <- 6 - dat$div

dat$n_1s <- sapply(dat$hands_char_mapped_J, \(x) sum(table(x) == 1))
dat$n_2s <- sapply(dat$hands_char_mapped_J, \(x) sum(table(x) == 2))

dat$type_signat <- dat$div_ord*100 + dat$n_1s*10 + dat$n_2s*1

dat

get_mapped_char <- function(i, ...) {
  sapply(dat$hands_char_mapped, \(x) x[i])
}
char_mapped <- as.data.frame(sapply(1:5, get_mapped_char))
colnames(char_mapped) <- paste0("C", 1:5)
dat <- cbind(dat, char_mapped)

dat_ordered <- dat[order(dat$type_signat,
                         dat$C1,
                         dat$C2,
                         dat$C3,
                         dat$C4,
                         dat$C5,
                         decreasing = T),]
dat_ordered$rank <- nrow(dat_ordered):1
dat_ordered
sum(dat_ordered$bids * dat_ordered$rank) # 6440

