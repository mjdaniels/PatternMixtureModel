#' Function to perform RAM_PMM
#'
#' It runs the nonignorable analyses for the RAM_PMM on the provided data
#' @param Dataset A data file having coulmns named and placed in a fixed order
#' @param Modelfile A WINBUGS model file
#' @param NumberofIndicators Number of Indicator Centers (Xs)
#' @param n.chains Parallel chains for the model
#' @param n.adapt iterations for adaptations
#' @param Intervention Column having name as Intervention
#' @param Response Column having name as Response
#' @param BaselineResponse  Column having name as BaselineResponse
#' @param Attempts Column hvaing name as Attempts
#' @return The summary of the posterior distribution.
#' @export
#' @import rjags
#' @import xtable
#'

RAM_PMM_fixed <- function(Dataset, Modelfile, NumberofIndicators= 7, n.chains = 2 , n.adapt = 1000, Intervention = "Z", Response = "Y2",
                          BaselineResponse = "Y1", Attempts = "R")

{

  dat <- utils::read.table(Dataset, header=T)

  fun1 <- function(x){1-sum(x)}
  X2tosecondlast <- as.matrix(cbind(dat[, 5:NumberofIndicators]))

  Xlast <- as.matrix(apply(X2tosecondlast, 1, fun1))

  Xcomp <- cbind(dat[[BaselineResponse]], X2tosecondlast, Xlast)

  L <- 2000
  L2 <- L*2
  M <- matrix(c(1.3333, -0.5, 0.3333, 0, -0.6667, 0.5), ncol=3)
  aarti <- rep(1, 4)

  input <- list("Y"=dat[[Response]], "R"=dat[[Attempts]], "Z"=dat[[Intervention]], "X"=Xcomp, "alex"=aarti, "M"=M, "K"=3, "C"=0, "L"=L, "L2"=L2)


  X_mc <- matrix(rep(c(39, 1, 0, 0, 0), L2), ncol=5, byrow=TRUE)

  init <- list(list(inv_tau=0.5, inv_sigma=0.5, X_mc=X_mc), list(inv_tau=0.8, inv_sigma=0.8, X_mc=X_mc))

  model <- jags.model(file= Modelfile , data=input, inits=init, n.chains, n.adapt)

  out <- coda.samples(model, variable.names=c("theta", "m0", "m1",
                                              "zeta_00", "zeta_10", "zeta_01", "zeta_11",
                                              "beta_0[1]", "beta_0[2]", "beta_0[3]",
                                              "alpha_00",
                                              "lambda_Z",
                                              "inv_tau",
                                              "inv_sigma"),
                                              n.iter=20000)
  return(summary(out))

}
