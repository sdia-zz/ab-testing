data {
	int<lower=0> N_A;
	int<lower=0> N_B;
	int<lower=0> n_A;
	int<lower=0> n_B;
}
parameters {
	real<lower=0,upper=1> theta_A;
	real<lower=0,upper=1> theta_B;
}
transformed parameters{
	real<lower=-1,upper=1> delta;
	delta <- theta_A-theta_B;
}
model {
	n_A ~ binomial(N_A, theta_A);
	n_B ~ binomial(N_B, theta_B);
	theta_A ~ beta(1, 2);
	theta_B ~ beta(1, 2);
	delta ~ normal(0, 10);
}