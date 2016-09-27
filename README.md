
<!-- README.md is generated from README.Rmd. Please edit that file -->
Q6
==

The goal of Q6 is to ...

Main Components of the package
------------------------------

The packages needs,

1.  a data .txt file.

2.  a WINBUGS code file that has a '' model in it. WINBUGS (Windows version of Bayesian inference Using Gibbs Sampling) is a MCMC software to implement Gibbs sampling, which generates samples from your posterior distribution. . Alternative to WINBUGS is JAGS that is accessed in R via rjags ( to do the Gibbs sampling). JAGS, written in C++, gets installed outide R. JAGS will need a model code text file for it to A regular model file has deterministic relations, stochastic relations through distributions suported in JAGS, and priors.

The jags.model() function sends the model in file JAGSmodel.txt to JAGS for parsing and compiling. When the JAGS model is compiled, the code syntax is turned into a set of sequential instructions and the compiler resolves the names and links in the model. The function generates 1000 samples (updates) of the MCMC algorithm.

Finally, the coda.samples() function generates posterior samples of and save them. It continues updating the chain for the number of iterations specified by n.iter, but this time it saves them if the parameter is listed in the variable.names argument. CODA stands for COnvergence Diagnostic and Analysis. It describes a set of functions for analyzing outputs generated from MCMC.

The object returned is of class mcmc.list. You use the summary() method on this object to obtain a summary of the posterior distribution. The output has the characteristics of your MCMC and then the statistics of the samples from the posterior distribution.

Installation
------------

You can install Q6 from github with:

``` r
# install.packages("devtools")
devtools::install_github("mjdaniels/RAM_PMM")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
## Run the package by following command
## RAM_PMM_fixed("data.txt", "model.txt")*
```
