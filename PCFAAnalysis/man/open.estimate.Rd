\name{open.estimate}
\alias{open.estimate}
\title{POPAN model fitting}
\usage{
  open.estimate(er, delta = Inf, alternate = FALSE,
    chat = 1)
}
\arguments{
  \item{er}{dataframe}

  \item{delta}{value of delta aicc to use for model
  averaging set. Default is Inf to use all.}

  \item{alternate}{If TRUE do not median center MT and use
  JS1 approach for abundance estimation}

  \item{chat}{Overdispersion value}
}
\value{
  List of results
}
\description{
  POPAN model fitting
}
\author{
  Jeff Laake
}

