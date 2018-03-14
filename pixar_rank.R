
setwd("C:/Users/ncitrone/Documents/IND/pixar")

pixar <- read.csv("pixar.csv", stringsAsFactors = FALSE)
df <- readRDS('movie_history.Rdata')
#df <- c("Toy Story 3", "Finding Nemo")
#df <- as.data.frame(t(df))
#colnames(df) <- c("winner", "loser")
#df 
df <- movie_many(df, number = 100)

#saveRDS(df, "movie_history.Rdata")

movie_rate <- function(df, movies=pixar){
  df_one <- df[1,]
  movie_id <- sample(x = c(1:nrow(movies)), size = 2, replace = FALSE)
  movie1 <- movies$movie[movie_id[1]]
  movie2 <- movies$movie[movie_id[2]]
  print(paste0("m1: ", movie1, " | m2: ", movie2))
  value = readline()
  if(value == 1){
    df_one$winner <- movie1
    df_one$loser <- movie2
    df <- rbind(df, df_one)
  } else if (value == 2){
    df_one$winner <- movie2
    df_one$loser <- movie1
    df <- rbind(df, df_one)
  }
  return(df)
}

movie_many <- function(df, movies = pixar, number = 10){
  for(jj in 1:number){
    df <- movie_rate(df)
    print(jj / number)
  }
  return(df)
}



library(dplyr)
df_summary <- df %>% group_by(winner) %>% summarize(
  wins = n()
)
df_loser <- df %>% group_by(loser) %>% summarize(
  losses = n()
)
colnames(df_loser) <- c("winner", "losses")
df_all <- left_join(df_summary, df_loser, by = "winner")
df_all <- mutate(df_all,
                 wins = ifelse(is.na(wins), 0, wins),
                 losses = ifelse(is.na(losses), 0, losses),
                 total = wins + losses,
                 winpct = wins/total
                 )

library(EloRating)

df$n <- as.Date(c(1:nrow(df)))
pixar_elo <- elo.seq(winner = df$winner, loser= df$loser, Date = df$n)
View(pixar_elo$lmat)
pixar_elo_values <- pixar_elo$lmat[nrow(pixar_elo$lmat),]
pev <- data.frame(winner = names(pixar_elo_values), elo = pixar_elo_values)

df_all <- left_join(df_all, pev, by = "winner")
df_all <- df_all[order(df_all$elo, decreasing = TRUE),]
df_all$rank <-c (1:nrow(df_all))

compare_matrix <- rep(0, nrow(pixar)^2)
dim(compare_matrix) <- c(19, 19)
compare_matrix <- as.data.frame(compare_matrix)
colnames(compare_matrix) <- pixar$movie
rownames(compare_matrix) <- pixar$movie
for(jj in 1:nrow(df)){
  winner_row <- match(df$winner[jj], rownames(compare_matrix))
  loser_column <- match(df$loser[jj], colnames(compare_matrix))
  compare_matrix[winner_row, loser_column] <- compare_matrix[winner_row, loser_column] +  1
  print(jj)
}

df_test <- df
df_test$name <- paste0(df_test$winner, ":", df_test$loser)
dfts <- df_test %>% group_by(name) %>% summarize(
  count = n(),
)
