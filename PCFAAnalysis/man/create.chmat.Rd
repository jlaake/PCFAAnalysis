\name{create.chmat}
\alias{create.chmat}
\title{Create dataframe with capture history matrix and related data}
\usage{
  create.chmat(ER)
}
\arguments{
  \item{ER}{dataframe containing all or subset of data in
  \code{\link{ER}}}
}
\value{
  A dataframe with the following fields: \item{ID}{CRC gray
  whale photo ID number} \item{ch}{character string of 0/1
  for the capture history} \item{cohort}{first year
  sighted} \item{times.resighted}{number of years resighted
  after initial sighting}
  \item{times.could.be.resighted}{number of years it could
  be resighted after initial sighting}
  \item{number.years.seen}{1+times.resighted}
  \item{minstay}{1+ number of days between first and last
  day seen in the first year seen}
  \item{min1998...min2007}{0 or the value of \code{minstay}
  if first year seen} \item{pmin1998...min2008}{value of
  \code{minstay} in the prior year regardless of number of
  years seen; pmin1998 always 0} \item{td1998...td2008}{0
  or 1 if seen in prior year; td2008 always 0}
  \item{first1998...first2008}{0 or 1 if that was the first
  year seen} \item{old}{0 or 1 if seen in a year prior to
  1998 in the ER data} \item{sightings}{number of sightings
  0 or 1 if seen in a year prior to 1998 in the ER data}
}
\description{
  Create dataframe with capture history matrix and related
  data
}
\author{
  Jeff Laake
}

