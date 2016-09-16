#' Function to perform RAM_PMM
#'
#' It runs the nonignorable analyses for the RAM_PMM on the provided dataset
#' @param  file having the data and model specifications
#' @return returns the summary
#' @export

RAM_PMM_fixed <- function(Dataset, Modelfile, NumberofIndicators)

{
  library(rjags)
  library(xtable)
  dat <- read.table(Dataset, header=T)

  fun1 <- function(x){1-sum(x)}
  X2tosecondlast <- as.matrix(cbind(dat[, 5:NumberofIndicators]))
  Xlast <- as.matrix(apply(X2tosecondlast, 1, fun1))
  Xcomp <- cbind(dat$Y1, X2tosecondlast, Xlast)

  L <- 2000
  L2 <- L*2
  M <- matrix(c(1.3333, -0.5, 0.3333, 0, -0.6667, 0.5), ncol=3)
  a <- rep(1, 4)

  input <- list("Y"=dat$Y2, "R"=dat$R, "Z"=dat$Z, "X"=Xcomp, "a"=a, "M"=M, "K"=3, "C"=0, "L"=L, "L2"=L2)

  X_mc <- matrix(rep(c(39, 1, 0, 0, 0), L2), ncol=5, byrow=TRUE)

  init <- list(list(inv_tau=0.5, inv_sigma=0.5, X_mc=X_mc), list(inv_tau=0.8, inv_sigma=0.8, X_mc=X_mc))

  model <- jags.model(file= Modelfile , data=input, inits=init, n.chains=2, n.adapt=1000)

  out <- coda.samples(model, variable.names=c("Diff", "m0", "m1", "zeta00", "zeta10", "zeta01", "zeta11",
                                              "delta0", "delta1", "alpha0_star", "alpha1_star"), n.iter=20000)
  return(summary(out))

}
