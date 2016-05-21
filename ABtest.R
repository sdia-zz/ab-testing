############################
### BAYESIAN A/B TESTING ###
######### Imad Ali #########
######## 05.20.2016 ########
############################

library(rstan)

# Specify true parameters
theta <- list()
theta$A <- 0.10
theta$B <- 0.05
theta$delta <- theta$A-theta$B

# Create data
test_data <- list()
test_data$N_A <- 140
test_data$n_A <- rbinom(1, test_data$N_A, theta$A)
test_data$N_B <- 210
test_data$n_B <- rbinom(1, test_data$N_B, theta$B)

# Create ABtest.stan, check compile, and run model

stanc("ABtest.stan")
fit <- stan("ABtest.stan", data = test_data, chains = 4, iter = 2000)
traceplot(fit)
print(fit)

# Extract parameter distributions

fit_extract <- extract(fit)
post <- list()
post$theta_A <- fit_extract$theta_A
post$theta_B <- fit_extract$theta_B
post$delta <- fit_extract$delta

# Plot

path <- getwd()

png(filename = paste0(path,"/images/post_plot.png"), width = 2000, height = 600, pointsize = 35, res = 85)
par(mfrow=c(1,3))

hist(post$theta_A, col = "grey", breaks = 50, border = "white", main = expression(paste("Histogram of ", theta["A"])), xlab = expression(theta["A"]))
abline(v=mean(post$theta_A), col = "red", lwd = 2)
text(0.15,250,bquote(theta["A"] ~ "=" ~ .(round(mean(post$theta_A),3))))

hist(post$theta_B, col = "grey", breaks = 50, border = "white", main = expression(paste("Histogram of ", theta["B"])), xlab = expression(theta["B"]))
abline(v=mean(post$theta_B), col = "red", lwd = 2)
text(0.10,150,bquote(theta["B"] ~ "=" ~ .(round(mean(post$theta_B),3))))

hist(post$delta, col = "grey", breaks = 50, border = "white", main = expression(paste("Histogram of ", delta)), xlab = expression(delta))
abline(v=mean(post$delta), col = "red", lwd = 2)
text(0.10,150,bquote(delta ~ "=" ~ .(round(mean(post$delta),3))))

dev.off()

# Probability that A is prefered to B and vice versa

post$prob <- c(
  length(post$delta[which(post$delta==0)]),
  length(post$delta[which(post$delta<0)]),
  length(post$delta[which(post$delta>0)])
  )

prop.table(post$prob)
