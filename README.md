# Bayesian A/B Testing using Stan

This repository contains an example of Bayesian A/B testing (i.e. marketing lingo for [randomized experiments](https://en.wikipedia.org/wiki/Randomized_experiment)) using [Stan](http://mc-stan.org/).

* `AB_test.R` contains the code to generate the data, run the Stan models, and plot the parameter distributions.
* `ABtest_uni.stan` contains the model for A/B testing using uninformative uniform priors.
* `ABtest_beta.stan` contains the model for A/B testing using beta(1,2) priors on the probability parameters and a truncated normal(0,10) prior with support [-1,1] for the difference in probabilities parameter.
