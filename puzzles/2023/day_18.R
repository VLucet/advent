library(advent)

input <- get_puzzle_input(2023, 18)

test_input <- c(
  "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"
) |> strsplit("\n") |> unlist()

parsed_df <- do.call(rbind, sapply(input, strsplit, " ", USE.NAMES = F)) |> 
  as.data.frame()
colnames(parsed_df) <- c("dir", "val", "color")
parsed_df_clean <- parsed_df |> 
  dplyr::mutate(val = as.numeric(val), 
                color = stringr::str_remove_all(color, pattern = "[()]"))

dir_map <- list(
  R = c(+0, +1),
  D = c(+1, +0), 
  L = c(+0, -1),
  U = c(-1, +0)
)

m_dim <- max(parsed_df_clean$val[parsed_df_clean$dir %in% c("D")], 
             sum(parsed_df_clean$val[parsed_df_clean$dir == c("R")])) * 2
m <- matrix(".", nrow = m_dim, ncol = m_dim)
pos <- round(c(nrow(m)/2,c(nrow(m)/2)))
# pos <- c(2,2)
m[pos[1], pos[2]] <- "#"

all_marks <- data.frame(pos[1], pos[2])
colnames(all_marks) <- c("x", "x")
for (i in 1:nrow(parsed_df_clean)) {
  marks <- sapply(1:parsed_df_clean[i,]$val, 
                  \(x) pos + (dir_map[[parsed_df_clean[i,]$dir]] * x)) |> t()
  colnames(marks) <- c("x", "x")
  all_marks <- rbind(all_marks, marks)
  m[marks] <- "#"
  pos <- marks[nrow(marks),]
}

# image(m=="#")
m <- m[-which(apply(m, MARGIN = 1, \(x) all(x=="."))), 
       -which(apply(m, MARGIN = 2, \(x) all(x==".")))]
all_marks[,1] <- all_marks[,1] - (pos[1]-1)
all_marks[,2] <- all_marks[,2] - (pos[2]-1)

prod_sum <- 0

for (i in 1:(nrow(all_marks) - 1)) {
  pos <- all_marks[i,]
  next_pos <- all_marks[i+1,]
  prod_sum <- prod_sum + ((pos[1] + next_pos[1]) * (pos[2] - next_pos[2]))
}
tot <- sum(m == "#")
area <- abs(prod_sum)/2
interior_count <- area + 1 - 0.5 * tot
total_vol <- interior_count + tot
total_vol
