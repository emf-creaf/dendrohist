% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_cedex_txt.R
\name{read_cedex_txt}
\alias{read_cedex_txt}
\title{Read cedex time series data}
\usage{
read_cedex_txt(x, type = NULL)
}
\arguments{
\item{x}{url corresponding to the text file to be fetched.}

\item{type}{character to specify whether the file contains 'daily' or 'monthly' data.}
}
\value{
A \code{data.frame} containing a time series. If \code{type="daily"} the \code{data.frame}
will have four columns named "Station", "Date", "Height", and "Flow".
if \code{type="monthly"} the \code{data.frame} will have three columns
named "Station", "Date" and "Flow".
}
\description{
\code{read_cedex_txt} reads time series data from the cedex site.
}
\details{
Datasets are read and their headers checked.
}
\examples{
x <- "https://ceh.cedex.es/anuarioaforos/temp/resultado8187.txt"
df <- read_cedex(x, type = "daily")
}
