trials_n <- 153
ID_n <- 40

set.seed(1238)

# Function for agent
SimpleBayesAgent <- function(FirstRating, GroupRating){
  SecondRating = round((FirstRating + GroupRating)/2)
  return(SecondRating)
}

# Simulating first ratings
FirstRating <- sample(1:8, size=trials_n*ID_n, replace=T)

# Simulating group ratings
GroupRating <- sample(1:8, size=trials_n*ID_n, replace=T)

# Create empty vectors
ID <- rep(NA, trials_n * ID_n)
trial <- rep(NA, trials_n * ID_n)
SecondRating <- rep(NA, trials_n * ID_n)

# Fill values correctly
index <- 1
for (j in 1:ID_n){
  for (i in 1:trials_n){
    SecondRating[index] <- SimpleBayesAgent(FirstRating[index], GroupRating[index])
    ID[index] <- j
    trial[index] <- i
    index <- index + 1
  }
}

df <- data.frame(ID, trial, FirstRating, GroupRating, SecondRating, 
                 Feedback = GroupRating - FirstRating, 
                 Change = SecondRating - FirstRating)

# Save to csv
write.csv(df, "data/simulated_simple_bayes.csv", row.names=F)
