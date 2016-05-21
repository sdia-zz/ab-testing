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

####################################
### A/B TEST WITH UNIFORM PRIORS ###
####################################

# Create ABtest.stan, check compile, and run model

stanc("ABtest_uni.stan")
fit1 <- stan("ABtest_uni.stan", data = test_data, chains = 4, iter = 2000)
traceplot(fit1)
print(fit1)

# Extract parameter distributions

fit1_extract <- extract(fit1)
post1 <- list()
post1$theta_A <- fit1_extract$theta_A
post1$theta_B <- fit1_extract$theta_B
post1$delta <- fit1_extract$delta

# Plot

path <- getwd()

png(filename = paste0(path,"/images/plot_uni.png"), width = 2000, height = 600, pointsize = 35, res = 85)
par(mfrow=c(1,3), oma = c(0,0,2,0))

hist(post1$theta_A, col = "grey", breaks = 50, border = "white", main = expression(paste("Histogram of ", theta["A"])), xlab = expression(theta["A"]), xlim = c(0,0.2))
abline(v=mean(post1$theta_A), col = "red", lwd = 2)
text(0.17,200,bquote(theta["A"] ~ "=" ~ .(round(mean(post1$theta_A),3))), cex = 0.7)

hist(post1$theta_B, col = "grey", breaks = 50, border = "white", main = expression(paste("Histogram of ", theta["B"])), xlab = expression(theta["B"]), xlim = c(0,0.2))
abline(v=mean(post1$theta_B), col = "red", lwd = 2)
text(0.15,200,bquote(theta["B"] ~ "=" ~ .(round(mean(post1$theta_B),3))), cex = 0.7)

hist(post1$delta, col = "grey", breaks = 50, border = "white", main = expression(paste("Histogram of ", delta)), xlab = expression(delta), xlim = c(-0.1,0.2))
abline(v=mean(post1$delta), col = "red", lwd = 2)
abline(v=0, lty = 2, lwd = 2)
text(0.15,200,bquote(delta ~ "=" ~ .(round(mean(post1$delta),3))), cex = 0.7)

mtext("A/B Testing with Uninformative (Uniform) Priors", outer = TRUE, cex = 0.8)

dev.off()

# Probability that A is prefered to B and vice versa

post1$prob <- c(
  length(post1$delta[which(post1$delta==0)]),
  length(post1$delta[which(post1$delta<0)]),
  length(post1$delta[which(post1$delta>0)])
  )

# Element 1 => no preference
# Element 2 => B preferred
# Element 3 => A preferred
prop.table(post1$prob)
