% This file is part of the 'muste' package for R
% This documentation (and function) borrows parts from
% the read.dta-function in the package foreign by Thomas Lumley 
% and R-core members

\name{read.svo}
\alias{read.svo}
\title{Read Survo Binary Files}
\description{
  Reads a file in Survo (or Muste) binary format into a data frame. 
}
\usage{
read.svo(file)
}
\arguments{
  \item{file}{a filename or URL as a character string.}
}
\details{
  If the filename appears to be a URL (of schemes \samp{http:},
  \samp{ftp:} or \samp{https:}) the URL is first downloaded to a
  temporary file and then read.  (\samp{https:} is only supported on
  some platforms.)
  
  The variables in the Survo data set become the columns of the data
  frame.  Missing values are correctly handled.  The variable labels, types
  and lengths as well as data description and info are stored as attributes
  of the data frame.
}
\value{
  A data frame with attributes.  These will include \code{"status.info"},
  \code{"status.description"}, \code{"status.varname"}, \code{"status.vartype"}
  and \code{"status.varlen"}.
}
\author{Reijo Sund}
%\examples{
%read.svo("TEST.SVO")
%}
\keyword{file}