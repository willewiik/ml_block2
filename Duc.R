library(randomForest)

# Assignment 1                          ####
random_trees <- function(ntree=1){
  # Result vector
  miss_class_error <- c()
  
  for(iter in 1:1000){
    # Generate train data
    x1<-runif(100)
    x2<-runif(100)
    trdata<-cbind(x1,x2)
    y<-as.numeric(x1<x2)
    trlabels<-as.factor(y)
    train <- data.frame(y = trlabels, x1, x2)
    
    # Fit the random forest model
    random_mod  <- randomForest(y ~ ., data=train, ntree=ntree,  nodesize=25, keep.forest=TRUE)
    
    # Random forest prediction on test data
    test_pred <- predict(random_mod, test_data)
    
    # Misclassification
    conf_mat <- table(test_data$y, test_pred)
    miss_class_error[iter] <- sum(diag(conf_mat)) / sum(conf_mat)
  }
  return(miss_class_error)
}
# Part 1                                ####
# Create test data
set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric(x1<x2)
telabels<-as.factor(y)
test_data <- data.frame(x1,x2,y=telabels)
plot(x1,x2,col=(y+1))


# Set seed in beginning to have reproducible result for markdown. 
set.seed(123)
random_1   <- random_trees(1)
random_10  <- random_trees(10)
random_100 <- random_trees(100)
mean(random_1)
sd(random_1)
mean(random_10)
sd(random_10)
mean(random_100)
sd(random_100)

# Part 2                                ####
# Create test data
set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric(x1<0.5)
telabels<-as.factor(y)
test_data <- data.frame(x1,x2,y=telabels)
plot(x1,x2,col=(y+1))

random_trees <- function(ntree=1){
  # Result vector
  miss_class_error <- c()
  
  for(iter in 1:1000){
    # Generate train data
    x1<-runif(100)
    x2<-runif(100)
    trdata<-cbind(x1,x2)
    y<-as.numeric(x1<0.5)
    trlabels<-as.factor(y)
    train <- data.frame(y = trlabels, x1, x2)
    
    # Fit the random forest model
    random_mod  <- randomForest(y ~ ., data=train, ntree=ntree,  nodesize=25, keep.forest=TRUE)
    
    # Random forest prediction on test data
    test_pred <- predict(random_mod, test_data)
    
    # Misclassification
    conf_mat <- table(test_data$y, test_pred)
    miss_class_error[iter] <- sum(diag(conf_mat)) / sum(conf_mat)
  }
  return(miss_class_error)
}

set.seed(123)
random_1   <- random_trees(1)
random_10  <- random_trees(10)
random_100 <- random_trees(100)
mean(random_1)
sd(random_1)
mean(random_10)
sd(random_10)
mean(random_100)
sd(random_100)

# Part 3                                ####
# Create test data
set.seed(1234)
x1<-runif(1000)
x2<-runif(1000)
tedata<-cbind(x1,x2)
y<-as.numeric( (x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5) )
telabels<-as.factor(y)
test_data <- data.frame(x1,x2,y=telabels)
plot(x1,x2,col=(y+1))

random_trees <- function(ntree=1){
  # Result vector
  miss_class_error <- c()
  
  for(iter in 1:1000){
    # Generate train data
    x1<-runif(100)
    x2<-runif(100)
    trdata<-cbind(x1,x2)
    y<-as.numeric( (x1<0.5 & x2<0.5) | (x1>0.5 & x2>0.5) )
    trlabels<-as.factor(y)
    train <- data.frame(y = trlabels, x1, x2)
    
    # Fit the random forest model
    random_mod  <- randomForest(y ~ ., data=train, ntree=ntree,  nodesize=25, keep.forest=TRUE)
    
    # Random forest prediction on test data
    test_pred <- predict(random_mod, test_data)
    
    # Misclassification
    conf_mat <- table(test_data$y, test_pred)
    miss_class_error[iter] <- sum(diag(conf_mat)) / sum(conf_mat)
  }
  return(miss_class_error)
}

set.seed(123)
random_1   <- random_trees(1)
random_10  <- random_trees(10)
random_100 <- random_trees(100)
mean(random_1)
sd(random_1)
mean(random_10)
sd(random_10)
mean(random_100)
sd(random_100)


# Assignment 2                          ####
set.seed(1234567890)
max_it <- 500 # max number of EM iterations (increased from 100)
min_change <- 0.1 # min change in log lik between two consecutive iterations
n=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=n, ncol=D) # training data

true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")
# Producing the training data
for(i in 1:n) {
  m <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[i,d] <- rbinom(1,1,true_mu[m,d])
  }
}

M=3 # number of clusters
w <- matrix(nrow=n, ncol=M) # weights
pi <- vector(length = M) # mixing coefficients
mu <- matrix(nrow=M, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations


# Random initialization of the parameters
pi <- runif(M,0.49,0.51)
pi <- pi / sum(pi)
for(m in 1:M) {
  mu[m,] <- runif(D,0.49,0.51)
}
for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  #points(mu[4,], type="o", col="yellow")
  Sys.sleep(0.1) # Decreased from 0.5
  # E-step: Computation of the  weights
  # Calculates the weights for each observation
  for(iter in 1:1000){
    x_d <- x[iter, ]
    
    # Bernolli for (x1 given mu_1), (x1 given mu_2), (x1 given mu_3)
    bern_mu1 <- prod(mu[1,]^x_d * (1 - mu[1, ])^(1-x_d))
    bern_mu2 <- prod(mu[2,]^x_d * (1 - mu[2, ])^(1-x_d))
    bern_mu3 <- prod(mu[3,]^x_d * (1 - mu[3, ])^(1-x_d))
    
    # Probabilities that the observation belongs to each distribution
    prob_1 <- pi[1] * bern_mu1 / sum(pi[1]*bern_mu1 + pi[2]*bern_mu2 + pi[3]*bern_mu3)
    prob_2 <- pi[2] * bern_mu2 / sum(pi[1]*bern_mu1 + pi[2]*bern_mu2 + pi[3]*bern_mu3)
    prob_3 <- pi[3] * bern_mu3 / sum(pi[1]*bern_mu1 + pi[2]*bern_mu2 + pi[3]*bern_mu3)
    
    # Probability that observation 1 comes from each distribution
    w[iter, ] <- c(prob_1, prob_2, prob_3) 
  }
  #Log likelihood computation.
  llik[it] <- sum(log(w))
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  flush.console()
  # Stop if the lok likelihood has not changed significantly
  # Your code here
  # M-step: ML parameter estimation from the data and weights
  # Calculate new pi
  pi <- 1/1000 * colSums(w)
  # Calculates new mu
  mu[1,] <- 1/sum(w[,1]) * colSums(w[,1] * x)
  mu[2,] <- 1/sum(w[,2]) * colSums(w[,2] * x)
  mu[3,] <- 1/sum(w[,3]) * colSums(w[,3] * x)
}
pi
mu
plot(llik[1:it], type="o")
# Algorithm should have stopped around 350.

# true_mu[1,] is our mu[3,] 
# true_mu[2,] is our mu[1,]
# true_mu[3,] is our mu[2,]




# Our own implementation algorithm      ####
# Calculates the weights for each observation
for(iter in 1:1000){
  x_d <- x[iter, ]
  
  # Bernolli for (x_iter|mu_1), (x_iter|mu_2), (x_iter|mu_3)
  bern_mu1 <- prod(mu[1,]^x_d * (1 - mu[1, ])^(1-x_d))
  bern_mu2 <- prod(mu[2,]^x_d * (1 - mu[2, ])^(1-x_d))
  bern_mu3 <- prod(mu[3,]^x_d * (1 - mu[3, ])^(1-x_d))
  
  # Probabilities that the observation belongs to each distribution
  prob_1 <- pi[1] * bern_mu1 / sum(pi[1]*bern_mu1 + pi[2]*bern_mu2 + pi[3]*bern_mu3)
  prob_2 <- pi[2] * bern_mu2 / sum(pi[1]*bern_mu1 + pi[2]*bern_mu2 + pi[3]*bern_mu3)
  prob_3 <- pi[3] * bern_mu3 / sum(pi[1]*bern_mu1 + pi[2]*bern_mu2 + pi[3]*bern_mu3)
  
  # Probability that observation "iter" comes from each distribution
  w[iter, ] <- c(prob_1, prob_2, prob_3) 
}

# Calculate new pi
pi <- 1/1000 * colSums(w)

# Calculates new mu
mu[1,] <- 1/sum(w[,1]) * colSums(w[,1] * x)
mu[2,] <- 1/sum(w[,2]) * colSums(w[,2] * x)
mu[3,] <- 1/sum(w[,3]) * colSums(w[,3] * x)

# Calculates log-likelihood
llik[1] <- sum(log(w))