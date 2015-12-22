#' S4 class definition for a linear model
#'
#' @slot model character vector description of model
#' @slot intercept is an intercept included in the model? Takes values TRUE or FALSE
#' @slot outcome numeric vector of the outcome ('y')
#' @slot covariates data frame of the covariates. At present must be numeric
#' @slot modelCoef matrix representation of the fitted model coefficients ('betahats')
#' @slot fittedVals numeric vector of the fitted values ('yhat')
#' @slot residuals numeric vector of the fitted residuals
#' @slot summaryStats list containing summary stats: Rsquared, weighted Rsquared and RMSE
#' @slot leverage numeric vector of the leverage of each fitted value
#' @export
setClass(
  Class = "LinRegP",
  slots = list(
    model = "character",
    intercept = "logical",
    outcome = "numeric",
    covariates = "data.frame",
    modelCoef = "matrix",
    fittedVals = "numeric",
    residuals = "numeric",
    summaryStats = "list",
    leverage = "numeric")
)


#' Initialiser for LinRegP objects
#' @param .Object object of class LinRegP
#' @param model character string representing model
#' @param intercept logical. is intercept included?
#' @param outcome numerical vector of the outcome
#' @param covariates data frame holding numerical covariates
#' @param modelCoef matrix of the model coefficients
#' @param fittedVals numerical vector of fitted values ('yhat')
#' @param residuals numerical vector of residuals
#' @param summaryStats list of summary statistics
#' @param leverage numerical vector of leverage of fitted values
#' @export
setMethod(
  f = "initialize",
  signature = "LinRegP",
  definition = function(.Object, model, intercept, outcome, covariates,
                        modelCoef, fittedVals, residuals, summaryStats,
                        leverage) {
    # cat("--- LinRegP: initialiser --- \n")
    colnames(modelCoef) <- c("betaHat", "stdErr", "t", "Pr(>|t|)")
    ifelse(intercept,
           rownames(modelCoef) <- c("Intercept", names(covariates)),
           rownames(modelCoef) <- names(covariates))

    .Object@model <- model
    .Object@intercept <- intercept
    .Object@outcome <- outcome
    .Object@covariates <- covariates
    .Object@modelCoef <- modelCoef
    .Object@fittedVals <- fittedVals
    .Object@residuals <- residuals
    .Object@summaryStats <- summaryStats
    .Object@leverage <- leverage
    return(.Object)
  }
)

#' Constructor for LinRegP
#' @param outcome character vector name of column containing outcome ("y")
#' @param covariates character vector of column names containing covariates ('x')
#' @param int logical. is an intercept included?
#' @param df data frame
#' @import MASS
#' @import methods
#' @import stats
#' @export
lmp <- function(outcome, covariates, int = TRUE, df) {

  y <- df[, names(df) == outcome]
  x <- df[, names(df) %in% covariates, drop = FALSE]
  if (int) x <- cbind(1, x)
  nVar <- ncol(x)
  X <- data.matrix(x)

  # 'betahat' equation
  XT <- t(X)
  XTX <- XT %*% X
  XTXi <- MASS::ginv(XTX)
  tmp <- XTXi %*% XT
  betaHat <- tmp %*% y

  # Hat matrix
  H <- X %*% tmp
  lever <- diag(H)

  # fitted vals
  yhat <- rowSums(simplify2array(Map(function(w, z) w * z,
                                     as.list(x), as.list(betaHat))))
  resids <-  y - yhat
  s2 <- sum((resids) ^2) / (length(y) - nVar )
  stdErr <- sqrt(diag(s2 * XTXi))
  tstats <- betaHat / stdErr
  pvals <- pt(abs(tstats), nrow(X) - nVar, lower.tail = FALSE) * 2
  rsq <- sum((yhat - mean(y)) ^2) / sum((y - mean(y)) ^2)
  rsqMod <- 1 - (1 - rsq) * ((nrow(X) - 1) / (nrow(X) - ncol(X)))
  rmse <- sqrt(mean((resids) ^2))

  return(new(Class = "LinRegP",
             model = (paste0(paste0(outcome, " ~ "), paste(covariates, collapse = " + "))),
             intercept = int,
             outcome = y,
             covariates = df[, names(df) %in% covariates, drop = FALSE],
             modelCoef = cbind(betaHat, stdErr, tstats, pvals),
             fittedVals = yhat,
             residuals = resids,
             summaryStats = list(Rsquared = rsq,
                                 ModRsquared = rsqMod,
                                 RMSE = rmse),
             leverage = lever))
}
