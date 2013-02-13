% This file is part of the 'muste' package for R
% This documentation (and function) borrows parts from
% the write.dta-function in the package foreign by Thomas Lumley 
% and R-core members

\name{write.svo}
\alias{write.svo}
\title{Write R Data Frame into Survo Binary File}
\description{
  Writes a data frame into Survo (or Muste) binary format. 
}
\usage{
write.svo(dataf,svofile)
}
\arguments{
  \item{dataf}{R data frame object}
  \item{svofile}{name for output file} 
}
\details{
  The R data frame is copied to the given Survo data set.
  Missing values and encodings are correctly handled.
}
\value{
  A Survo data file.
}
\author{Reijo Sund}
%\examples{
%write.svo(test,"TEST.SVO")
%}
\keyword{file}