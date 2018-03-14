## Install Necessary Packages
install.packages('dpylr')
install.packages('EloRating')
library(dplyr)
library(EloRating)

## Read in Data & Initialize dataframe
pixar <- read.csv("pixar.csv", stringsAsFactors = FALSE)
#df <- readRDS('movie_history.Rdata')
df <- c("Toy Story 3", "Cars 2")
df <- as.data.frame(t(df))
colnames(df) <- c("winner", "loser")

## Run Functions to Compare Movies
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
########################################################

## IF YOU HAVE MADE PREVIOUS PICKS AND SAVED THEM
## Then reload them by removing the # and running the line below
# df <- readRDS("my_picks.Rdata")

## HERE IS WHERE YOU RUN THE FUNCTION TO ADD TO YOUR PICK HISTORY (DF)
df <- movie_many(df, number = 100) # Run this to make 100 Picks
## For each selection, you will see m(ovie)1: etc. | m2: etc.
## Type 1 then hit enter for Movie1, Type 2 then hit enter for Movie2
## Re-run the same bid of code above to enter another 100 movies.

## Run this line to save your picks so far!
saveRDS(df, "my_picks.Rdata")

## Run this Code to get the Summary Table of Picks So Far
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

df$n <- as.Date(c(1:nrow(df)))
pixar_elo <- elo.seq(winner = df$winner, loser= df$loser, Date = df$n)
pixar_elo_values <- pixar_elo$lmat[nrow(pixar_elo$lmat),]
pev <- data.frame(winner = names(pixar_elo_values), elo = pixar_elo_values)

df_all <- left_join(df_all, pev, by = "winner")
df_all <- df_all[order(df_all$elo, decreasing = TRUE),]
df_all$rank <-c (1:nrow(df_all))
# Run Code down to this line to generate updated Rankings

## Code to View rankings:
View(df_all)

