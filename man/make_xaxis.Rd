% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make-xaixs.R
\name{make_xaxis}
\alias{make_xaxis}
\title{Create x-axis}
\usage{
make_xaxis(
  at,
  at_minor = NULL,
  xlab = NULL,
  x0 = 1,
  x_trans = "none",
  ticks_digits = 1,
  gp = gpar(),
  xlab_gp = NULL,
  xlim
)
}
\arguments{
\item{at}{Numerical vector, create ticks at given values.}

\item{at_minor}{Numerical vector, create ticks at given values without label.}

\item{xlab}{X-axis labels, it will be put under the x-axis. An atomic vector should
be provided if different \code{xlab} for different column is desired.}

\item{x0}{Position of vertical line for 0 or 1.}

\item{x_trans}{Change axis scale, Allowed values are one of c("none", "log", "log2",
"log10"). Default is \code{"none"}, no transformation will be applied.
The formatted label will be used for \code{scale  = "log2"} or \code{"log10"}, change
this in \code{x_trans}. Set this to \code{"log"} if x-axis tick marks assume values
are exponential, e.g. for logistic regression (OR), survival estimates (HR), Poisson
regression etc.}

\item{ticks_digits}{Number of digits for the x-axis, default is \code{NULL} to calculate
an integer based on \code{ticks_at} if provided or \code{lower} and \code{upper} if not.
This should be a numerical vector if different rounding will be applied to different
column. If an integer is specified, for example \code{1L}, trailing zeros after
the decimal mark will be dropped. Specify numeric, for example  \code{1}, to keep
the trailing zero after the decimal mark.}

\item{gp}{Graphical parameters for arrow.}

\item{xlab_gp}{Graphical parameters for xlab.}

\item{xlim}{Limits for the x axis as a vector of length 2, i.e. c(low, high). It
will take the minimum and maximum of the lower and upper value if not provided.
This will apply to all CI columns if provided, and will be calculated automatically
for each column if not provided. This should be a list with the same length of
\code{ci_column} if different \code{xlim} for different column is desired.}
}
\value{
A grob
}
\description{
This function used to x-axis for the forest plot.
}
\keyword{internal}
