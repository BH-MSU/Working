% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StepReg.R
\name{recom}
\alias{recom}
\title{Recommend best set of data transforming parameters}
\usage{
recom(pred, resp, df, type, fit = NULL, st.row)
}
\arguments{
\item{pred}{A character, name of the predictor to be inserted to the model.}

\item{resp}{A character, name of the response variable in the model.}

\item{df}{A data.frame contains the predictor and response variables.}

\item{type}{The transformation method for the predictor. 1 means test all combinations of parameters of carryover & s-curve; 2 means test all combinations of parameters of carryover & power curve; 3 means do both of the actions above}

\item{fit}{A linear regression model. The default value is NULL.}

\item{st.row}{Numeric, number of modeling starting row.}
}
\value{
A numeric vector of length 4, contains the recommended parameters for transformation.
}
\description{
Check the best R-square / p-value of coeficients within the range set of transforming parameters, give recommendation of parameter set
}

