#### Datasets ####

# Loading data: train, test
dat.train <- read.csv('data/train.csv')
dat.test  <- read.csv('data/submit.csv')
dat <- list(train = dat.train, test = dat.test)

# "type" is the response



#### Helpers ####

# data.frame, Chars -> data.frame
# Drop predictors
dropPredictor <- function(df, drops) {
    df[ , - which(names(df) %in% drops)]
}


#### Manipulation ####

# Drop ID
dat <- lapply(dat, dropPredictor, "id")