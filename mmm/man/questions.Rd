% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StepReg.R
\name{questions}
\alias{questions}
\title{Questions}
\usage{
questions(index, pred = NULL, df = NULL, coef = NULL, opt = NULL)
}
\arguments{
\item{index}{Number of question to be used.}

\item{pred}{A character, name of variable to be transformed.}

\item{df}{A data.frame to store the variable transformed (original variable data will be replaced).}

\item{coef}{A data.frame, form of fit$coef.}

\item{opt}{The option for the upper question.}
}
\value{
The answer option for the question.
}
\description{
The variable is replaced by the transformed data using carry over and S-curve or power-curve transformation.
}

