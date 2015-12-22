#' @include class-definition.R
NULL

#' Show generic
#' @param object object of class LinRegP
#' @export
setMethod(
  f = "show",
  signature = "LinRegP",
  function(object) {
    cat("\nCall: \n")
    cat(object@model)
    cat("\n\nCoefficients: \n \n")
    print(object@modelCoef)
  }
)

#' Summary generic
#' @param object object of class LinRegP
#' @export
setMethod(
  f = "summary",
  signature = "LinRegP",
  definition = function(object) {
    cat("\nCall: \n")
    cat(object@model)
    cat("\n\nResiduals:\n")
    print(summary(object@residuals))
    cat("\nCoefficients: \n")
    print(object@modelCoef)
    cat("\nRsquared: ")
    cat(object@summaryStats[[1]])
    cat("\nAdjusted Rsquared: ")
    cat(object@summaryStats[[2]])
    cat("\nRMSE: ")
    cat(object@summaryStats[[3]])
  }
)

#' Plot generic. Note that do not use standardised residuals as the S3
#' lm class does
#' @param x plotting param
#' @param y plotting param
#' @param ... all other arguments
#'@export
setMethod(
  f = "plot",
  signature = "LinRegP",
  definition = function(x, y, ...) {
    .pardefault <- par(no.readonly = TRUE)
    par(mfcol=c(2, 2),
        mar = c(4, 4, 2, 1) + 0.1)
    plot(x@outcome ~ x@fittedVals,
         xlab = "Fitted",
         ylab = "Actual",
         xlim = c(min(range(x@outcome)[1], range(x@fittedVals)[1]),
                  max(range(x@outcome)[2], range(x@fittedVals)[2])),
         main = "Fitted vs Actual")
    abline(0, 1)
    plot(x@residuals ~ seq(length(x@residuals)),
         ylim = c(-max(abs(range(x@residuals))), max(abs(range(x@residuals)))),
         xlab = "Sample #",
         ylab = "Residual",
         main = "Residuals")
    abline(0, 0)
    qqnorm(x@residuals)
    plot(x@residuals~ x@leverage,
         xlab = "Leverage",
         ylab = "Residuals",
         main = "Leverage",
         ylim = c(-max(abs(range(x@residuals))), max(abs(range(x@residuals))))
    )
    par(.pardefault)
  }
)

#' Predict generic
#' @param object object of class LinRegP
#' @param newdata new data to predict outcome
#' @param ... any other arguments
#' @export
setMethod(
  f = "predict",
  signature = "LinRegP",
  definition = function(object, newdata,  ...) {
    if(object@intercept) newdata <- cbind(1, newdata)
    rowSums(simplify2array(Map(function(x, y) x * y,
                               as.list(newdata), as.list(object@modelCoef[, 1]))))
  }
)
