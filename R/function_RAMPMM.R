#' Function to perform Pattern Mixture Model for the Repeated Attempt Models (RAM-PMM)
#'
#' It runs the nonignorable analyses of the RAM-PMM approach on a dataset provided by the user
#'
#' @param Dataset A data file ( txt/csv) having coulmns that are named and placed in a specific order.
#' @param Modelfile A JAGS model file that rjags will use to simulate a model via JAGS installed at User's system.
#' @param NumberofIndicators Number of Indicator Centers (Xs). The User dataset must have Xs starting from 5th column and ending as the last column of the dataset.
#' @param n.chains Parallel chains for the model
#' @param n.adapt The number of iterations for adaptation.
#' @param Intervention Column having values for 'Intervention'. Users must either have name 'Z' for this column in their data or provide the name for this column as per their dataset.
#' @param Response Column having values for 'Response'. Users must either have name 'Y2' for this column in their data or provide the name for this column as per their dataset.
#' @param BaselineResponse  Column having values for 'Baseline Response'. Users must either have name 'Y1' for this column in their data or provide the name for this column as per their dataset.
#' @param Attempts Column having values 'Number of Attempts'. Users must either have name 'R' for this column in their data or provide the name for this column as per their dataset.
#' @return The summary of the posterior distribution. User will get an object named 'Results' in the global environment. Corresponding values for any particular column can be obtained by Results$columnname.
#' @export
#' @import rjags
#' @import xtable
#' @import utils

PMM <- function(Dataset, Modelfile, Response = "Y2", BaselineResponse = "Y1", Attempts = "R", Intervention = "Z", NumberofIndicators= 3, n.chains = 2 , n.adapt = 1000)
{
  dat <- read.table(Dataset, header=T)

  if(is.null(dat[[Attempts]]) | is.null(dat[[Response]]) | is.null(dat[[BaselineResponse]]) | is.null(dat[[Intervention]])){
    e <- simpleError("Could not find one of the argument columns. Please provide the columns in the right order and with the right name")
    stop(e)
    }

  ZYXR <- 4 #First 4 Columns must be Intervention, Baseline Response, Outcome, and Number of Attempts. Any Order is acceptable.

  lastcolumn <- ZYXR + NumberofIndicators

  fun1 <- function(x){1-sum(x)}

  X2tosecondlast <- as.matrix(cbind(dat[, 5:lastcolumn]))

  Xlast <- as.matrix(apply(X2tosecondlast, 1, fun1))

  Xcomp <- cbind(dat[[BaselineResponse]], X2tosecondlast, Xlast)

  L <- 2000
  L2 <- L*2
  M <- matrix(c(1.3333, -0.5, 0.3333, 0, -0.6667, 0.5), ncol=3)
  a <- rep(1, 4)

  input <- list("Y"=dat[[Response]], "R"=dat[[Attempts]], "Z"=dat[[Intervention]], "X"=Xcomp, "a"=a, "M"=M, "K"=3, "C"=0, "L"=L, "L2"=L2)

  X_mc <- matrix(rep(c(39, 1, 0, 0, 0), L2), ncol=5, byrow=TRUE)

  init <- list(list(inv_tau=0.5, inv_sigma=0.5, X_mc=X_mc), list(inv_tau=0.8, inv_sigma=0.8, X_mc=X_mc))

  model <- rjags::jags.model(file= Modelfile , data=input, inits=init, n.chains, n.adapt)

  out <- rjags::coda.samples(model, variable.names=c("theta", "m0", "m1", "zeta_00", "zeta_10", "zeta_01", "zeta_11"), n.iter=20000)

  Results <- NULL
  Results <<- summary(out)
  print("The posterior summaries are stored in the 'Results' Variable (Object type 'list'). Examples Usage: Result$statistics")

}

