% This file is part of the 'virta' package for R

\name{virta.sql}
\alias{virta.sql}
\title{SQL queries from Muste (SVO)-files using virtual tables}
\description{
  Makes it possible to easily utilize Muste (SVO)-files in SQL-queries and
  also move output results directly to SVO-files. 
}
\usage{
virta.sql(query,destfile,database,encod)
}
\arguments{
  \item{query}{SQL query}
  \item{destfile}{path to SVO-output file}
  \item{database}{path to RSQLite database file to be used (defaults to memory)}
  \item{encod}{force enconding (defaults to UTF-8)}
}
\details{
  Initializes database connection, enables virtual tables (references to SVO-files
  in queries must be given as MUSTE.<path to SVO-file>), runs the query, returns
  R data frame or outputs SVO-file.
}
\value{
  R data frame or SVO-file.
}
\author{Reijo Sund}
%\examples{
%res<-virta.sql("select Kunta from MUSTE.<Survo>/KUNNAT")
%}
\keyword{sql}