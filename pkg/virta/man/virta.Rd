\name{virta}
\alias{virta}
\docType{package}
\title{
Survo virtual tables for RSQLite
}
\description{
  
Support for using Survo data as SQLite's virtual table using loadable extension feature.

}
\section{Available virtual tables}{
  You can enable all of the following virtual tables by calling the
  \code{init_virtualtables} function on an RSQLite database connection
  object.

}

\author{
Reijo Sund <reijo.sund@uef.fi>
Maintainer: Reijo Sund <reijo.sund@uef.fi>
}

\keyword{ package }

\examples{
## basic usage looks like this
db <- dbConnect(SQLite(), dbname = ":memory:")
init_virtualtables(db)

## now you can use virtual tables in SQL queries.
dbDisconnect(db)
}
