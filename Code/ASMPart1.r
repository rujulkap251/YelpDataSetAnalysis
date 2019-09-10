###################Part - 1######################



library("jsonlite")
library("ggplot2")
library("readr")

df_bus_TO <- stream_in(file("https://www.scss.tcd.ie/~arwhite/Teaching/CS7DS3/Yelp%20data/Business_Toronto_Restaurant.json"))

class(df_bus_TO$categories)

head(df_bus_TO$categories)
cat_total <- unlist(df_bus_TO$categories)

length(cat_total)

cat_total <- factor(cat_total)

nlevels(cat_total)
cat_names_sort <- sort(table(cat_total), decreasing = TRUE)

head(cat_names_sort, n = 60)

tail(cat_names_sort, n = 25)

cat_names <- names(cat_names_sort)[2:60] ## 1 is Restaurants - we don't need this

cat_bus_ind_mat <- sapply(df_bus_TO$categories, function(y) as.numeric(cat_names %in% y))
cat_bus_ind_mat <- t(cat_bus_ind_mat)
colnames(cat_bus_ind_mat) <- cat_names
df_TO_tidy_cat <- cbind(df_bus_TO, cat_bus_ind_mat)
df_TO_tidy_cat$Indian <- as.factor(df_TO_tidy_cat$Indian)

sel_ck <- subset(df_TO_tidy_cat, (neighborhood == "Etobicoke" | neighborhood == "Scarborough") & Indian==1 & is_open==1, select =  c("stars", "neighborhood","Indian","is_open"))
dim(sel_ck)

library(ggplot2)
ggplot(sel_ck) + geom_boxplot(aes(neighborhood, stars, fill = neighborhood)) + geom_jitter(aes(neighborhood, stars, shape = ))
#sel_ck$Indian <- NULL

tapply(sel_ck$stars, sel_ck$neighborhood, mean)
tapply(sel_ck$stars, sel_ck$neighborhood, median)
tapply(sel_ck$stars, sel_ck$neighborhood, sd)


dim(sel_ck)
sum(sel_ck$neighborhood == "Scarborough")
sum(sel_ck$neighborhood == "Etobicoke")

sel_ck
#We know that the rating are between 0 to 5,
mu0=2.5
tau0 = 1/(1.125)^2#, , , , 
del0 = 0
gamma0 = 1/(1.25)^2
a0 = 2
b0 = 2*(1.25)^2

dim(sel_ck)

sel_ck$neighno <- ifelse(sel_ck$neighborhood == "Scarborough", 1, 2)
sel_ck$neighno <- as.factor(sel_ck$neighno)
ggplot(sel_ck) + geom_boxplot(aes(neighborhood, stars, fill = neighborhood)) + geom_jitter(aes(neighborhood, stars, shape = neighborhood))

tapply(sel_ck$stars, sel_ck$neighno, mean)
tapply(sel_ck$stars, sel_ck$neighno, median)
tapply(sel_ck$stars, sel_ck$neighno, sd)

t.test(stars ~ neighborhood, data=sel_ck, var.equal = TRUE)

compare_2_gibbs <- function(y, ind, mu0 = 2.5, tau0 = 1/(1.125)^2, del0 = 0, gamma0 = 1/(1.125)^2, a0 = 2, b0 = 2*(1.125)^2, maxiter = 5000)
{
  y1 <- y[ind == 1]
  y2 <- y[ind == 2]
  
  n1 <- length(y1) 
  n2 <- length(y2)
  
  ##### starting values
  mu <- (mean(y1) + mean(y2)) / 2
  del <- (mean(y1) - mean(y2)) / 2
  
  mat_store <- matrix(0, nrow = maxiter, ncol = 3)
  #####
  
  ##### Gibbs sampler
  an <- a0 + (n1 + n2)/2
  
  for(s in 1 : maxiter) 
  {
    
    ##update tau
    bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
    tau <- rgamma(1, an, bn)
    ##
    
    ##update mu
    taun <-  tau0 + tau * (n1 + n2)
    mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
    mu <- rnorm(1, mun, sqrt(1/taun))
    ##
    
    ##update del
    gamman <-  gamma0 + tau*(n1 + n2)
    deln <- ( del0 * gamma0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
    del<-rnorm(1, deln, sqrt(1/gamman))
    ##
    
    ## store parameter values
    mat_store[s, ] <- c(mu, del, tau)
  }
  colnames(mat_store) <- c("mu", "del", "tau")
  return(mat_store)
}
apply(fit, 2, mean)
apply(fit, 2, sd)
mean(sqrt(1/(fit[, 3])^2))
sd(sqrt(1/(fit[, 3])^2))

mean(sqrt(1/(fit[, 2])^2))
sd(sqrt(1/(fit[, 2])^2))


mean(1/sqrt(fit[, 2]))

install.packages("MCMCpack")

library(MCMCpack)
fit <- compare_2_gibbs(sel_ck$stars, as.factor(sel_ck$neighno))
plot(as.mcmc(fit))



Scarborough <- rnorm(5000, fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3]))

Etobicoke <- rnorm(5000, fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))

ggplot(data.frame(y_sim_diff = Scarborough - Etobicoke)) + stat_bin(aes(y_sim_diff))

mean(Scarborough > Etobicoke)

?tapply
#y1_sim.new = y1_sim[seq(1, length(y1_sim), 10)]
#y2_sim.new = y2_sim[seq(1, length(y2_sim), 10)]
#mean(y1_sim.new > y2_sim.new)
mean(Scarborough) - mean(Etobicoke)
ggplot(data.frame(Scarborough, Etobicoke)) + geom_point(aes(Scarborough, Etobicoke), alpha = 0.3) + geom_abline(slope = 1, intercept = 0)

#Comparing 2 means
#Taking separte dataset for open restaurant
sel_open_all <- subset(df_TO_tidy_cat, is_open==1, select =  c("stars", "neighborhood","is_open"))
dim(sel_open_all)
head(sel_open_all)

a <- sel_open_all$neighborhood == ""
sel_open_all2 <- sel_open_all[!a,]
dim(sel_open_all2)

sel_open_all2$neighborhood <- factor(sel_open_all2$neighborhood)

sel_open_all2$index <- as.numeric(as.factor(sel_open_all2$neighborhood))
newdata <- sel_open_all2[order(sel_open_all2$index),] 
ggplot(newdata) + geom_boxplot(aes(x = reorder(index, stars, median), stars, fill = reorder(index, stars, median)), show.legend=FALSE)
newdata[newdata$index ==54,]
newdata <- newdata[!newdata$neighborhood=="Meadowvale Village",]
newdata <- newdata[!newdata$neighborhood=="Cooksville",]
newdata <- newdata[!newdata$neighborhood=="Ossington Strip",]

ggplot(newdata, aes(x = reorder(index, index, length))) + stat_count()

ggplot(sel_open_all2, aes(stars)) + stat_bin()

ggplot(data.frame(size = tapply(newdata$stars, newdata$index, length), mean_score = tapply(newdata$stars, newdata$index, mean)), aes(size, mean_score)) + geom_point()

###############
compare_m_gibbs <- function(y, ind, maxiter = 2){
  
  
  ### weakly informative priors
  a0 <- 2 ; b0 <- 2*(1.125)^2 ## tau_w hyperparameters
  eta0 <- 2 ; t0 <- 2*(1.125)^2 ## tau_b hyperparameters
  mu0<-2.5 ; gamma0 <- 1/(1.125)^2
  ###
  
  ### starting values
  m <- nlevels(ind)
  ybar <- theta <- tapply(y, ind, mean)
  tau_w <- mean(1 / tapply(y, ind, var)) ##within group precision
  mu <- mean(theta)
  tau_b <-var(theta) ##between group precision
  n_m <- tapply(y, ind, length)
  an <- a0 + sum(n_m)/2
  print('test:',paste0(tapply(y, ind, var)))
  print(paste0('m: ',m, ' ybar:', ybar, ' tau_w:',tau_w,' mu:',mu, ' tau_b:',tau_b, ' n_m: ',n_m, ' an: ',an))
  ###
  print(paste0('m :',m))
  ### setup MCMC
  theta_mat <- matrix(0, nrow=maxiter, ncol=m)
  mat_store <- matrix(0, nrow=maxiter, ncol=3)
  ###
  
  ### MCMC algorithm
  for(s in 1:maxiter) 
  {
    
    # sample new values of the thetas
    for(j in 1:m) 
    {
      taun <- n_m[j] * tau_w + tau_b
      thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
      theta[j]<-rnorm(1, thetan, 1/sqrt(taun))
    }
    
    #sample new value of tau_w
    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((y[ind == j] - theta[j])^2)
    }
    bn <- b0 + ss/2
    print(paste0('ss :', ss))
    tau_w <- rgamma(1, an, bn)
    
    #sample a new value of mu
    gammam <- m * tau_b + gamma0
    mum <- (mean(theta) * m * tau_b + mu0 * gamma0) / gammam
    mu <- rnorm(1, mum, 1/ sqrt(gammam)) 
    
    # sample a new value of tau_b
    etam <- eta0 + m/2
    tm <- t0 + sum((theta-mu)^2)/2
    tau_b <- rgamma(1, etam, tm)
    print(paste0('tau_b: ',tau_b))
    #store results
    theta_mat[s,] <- theta
    mat_store[s, ] <- c(mu, tau_w, tau_b)
  }
  colnames(theta_mat) <- levels(ind)
  colnames(mat_store) <- c("mu", "tau_w", "tau_b")
  return(list(params = mat_store, theta = theta_mat))
}

class(sel_open_all2$index[1])

compare_m_gibbs <- function(y, ind, maxiter = 5000)
{
  
  ### weakly informative priors
  a0 <- 1/2 ; b0 <- 50 ## tau_w hyperparameters
  eta0 <-1/2 ; t0 <- 50 ## tau_b hyperparameters
  mu0<-50 ; gamma0 <- 1/25
  ###
  
  ### starting values
  m <- nlevels(ind)
  ybar <- theta <- tapply(y, ind, mean)
  tau_w <- mean(1 / tapply(y, ind, var)) ##within group precision
  mu <- mean(theta)
  tau_b <-var(theta) ##between group precision
  n_m <- tapply(y, ind, length)
  an <- a0 + sum(n_m)/2
  print('test:',paste0(tapply(y, ind, var)))
  print(paste0('m: ',m, ' ybar:', ybar, ' tau_w:',tau_w,' mu:',mu, ' tau_b:',tau_b, ' n_m: ',n_m, ' an: ',an))
  
  ###
  
  ### setup MCMC
  theta_mat <- matrix(0, nrow=maxiter, ncol=m)
  mat_store <- matrix(0, nrow=maxiter, ncol=3)
  ###
  
  ### MCMC algorithm
  for(s in 1:maxiter) 
  {
    
    # sample new values of the thetas
    for(j in 1:m) 
    {
      taun <- n_m[j] * tau_w + tau_b
      thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
      theta[j]<-rnorm(1, thetan, 1/sqrt(taun))
    }
    
    #sample new value of tau_w
    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((y[ind == j] - theta[j])^2)
    }
    bn <- b0 + ss/2
    tau_w <- rgamma(1, an, bn)
    
    #sample a new value of mu
    gammam <- m * tau_b + gamma0
    mum <- (mean(theta) * m * tau_b + mu0 * gamma0) / gammam
    mu <- rnorm(1, mum, 1/ sqrt(gammam)) 
    
    # sample a new value of tau_b
    etam <- eta0 + m/2
    tm <- t0 + sum((theta-mu)^2)/2
    tau_b <- rgamma(1, etam, tm)
    
    #store results
    theta_mat[s,] <- theta
    mat_store[s, ] <- c(mu, tau_w, tau_b)
  }
  colnames(mat_store) <- c("mu", "tau_w", "tau_b")
  return(list(params = mat_store, theta = theta_mat))
}


plot(as.mcmc(fit2$theta[,3]), abline(v=3.5, col="blue"))

for(j in 1:m){
  mu <- rnorm(5000, theta,) 
}

ggplot(fit2) + geom_boxplot(aes(x = reorder(school, mathscore, median), mathscore, fill = reorder(school, mathscore, median)), show.legend=FALSE)

fit2$theta

nlevels(newdata$index)
newdata$index <- as.factor(newdata$index)
fit2 <- compare_m_gibbs(newdata$stars, newdata$index)

apply(fit2$params, 2, mean)

apply(fit2$params, 2, sd)


mean(1/sqrt(fit2$params[, 3]))


sd(1/sqrt(fit2$params[, 3]))

theta_hat <- apply(fit2$theta, 2, mean)

ggplot(data.frame(size = tapply(newdata$stars, newdata$index, length), theta_hat = theta_hat), aes(size, theta_hat)) + geom_point()

which(theta_hat==max(theta_hat))
theta_hat

sort(theta_hat, decreasing = TRUE)
newdata[newdata$index==51,]

fit(theta[,43],)
mean(fit2$theta[,36] > mean(theta_hat))

theta_hat






