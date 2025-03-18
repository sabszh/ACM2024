trials_n <- 153
ID_n <- 40

# Function for agent
SimpleBayesAgent <- function(FirstRating, GroupRating) {
  round((FirstRating + GroupRating) / 2)
}

# Create a data frame with all ID and trial combinations
df <- expand.grid(ID = 1:ID_n, trial = 1:trials_n)

# Generate random ratings
df$FirstRating <- sample(1:8, size = nrow(df), replace = TRUE)
df$GroupRating <- sample(1:8, size = nrow(df), replace = TRUE)

# Compute SecondRating using vectorized operations
df$SecondRating <- mapply(SimpleBayesAgent, df$FirstRating, df$GroupRating)

# Compute Feedback and Change
df$Feedback <- df$GroupRating - df$FirstRating
df$Change <- df$SecondRating - df$FirstRating