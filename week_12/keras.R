# Time Series Forecasting with Recurrent Neural Networks
# https://tensorflow.rstudio.com/blog/time-series-forecasting-with-recurrent-neural-networks.html

library(tibble)
library(readr)
library(ggplot2)
#The data is recorded every 10 minutes.
#Thus, 144 data points are collected in each day.
data <- read.csv("jena_climate_2009_2016.csv",header = TRUE)
str(data)
#year scale
ggplot(data, aes(x = 1:nrow(data), y = `T..degC.`)) + geom_line()
# a scale of days (10 days)
ggplot(data[1:1440,], aes(x = 1:1440, y = `T..degC.`)) + geom_line()

#If you were trying to predict average temperature for the next month given a few months of 
#past data, the problem would be easy, due to the reliable year-scale periodicity of the data. 
#But looking at the data over a scale of days, the temperature looks a lot more chaotic. 
#Is this time series predictable at a daily scale? Let??s find out.

#Discard the first column because the it is text type.
data <- data.matrix(data[,-1])

#Step 1, normalize the data.
#Use the first 200000 timesptes as training data for the normalization of all data points.
train_data <- data[1:200000,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
#Use scale() function and the training data to normalize the whole data

#?˴?scale???\??->?N???ƪ??????ܦ?0?B?зǮt?ܦ?1?C
#apply(scale(data, center = apply(data, 2, mean), scale = apply(data, 2, sd)), 2, mean)
#apply(scale(data, center = apply(data, 2, mean), scale = apply(data, 2, sd)), 2, sd)

data <- scale(data, center = mean, scale = std)





generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows), 
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1, 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }            
    
    list(samples, targets)
  }
}


lookback <- 1440
step <- 6
delay <- 144
batch_size <- 128

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 200000,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 200001,
  max_index = 300000,
  step = step,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 300001,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- (300000 - 200001 - lookback) / batch_size

# How many steps to draw from test_gen in order to see the entire test set
test_steps <- (nrow(data) - 300001 - lookback) / batch_size


library(keras)
evaluate_naive_method <- function() {
  batch_maes <- c()
  for (step in 1:val_steps) {
    c(samples, targets) %<-% val_gen()
    preds <- samples[,dim(samples)[[2]],2]
    mae <- mean(abs(preds - targets))
    batch_maes <- c(batch_maes, mae)
  }
  print(mean(batch_maes))
}

evaluate_naive_method()



