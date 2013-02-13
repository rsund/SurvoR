\name{virta}
\docType{package}
\title{
Virtual tables for RSQLite
}
\description{
  
This package uses SQLite's loadable extension feature to provide
virtual tables.

}
\section{Available virtual tables}{
  You can enable all of the following virtual tables by calling the
  \code{init_virtualtables} function on an RSQLite database connection
  object.

}

\author{
Reijo Sund <reijo.sund@helsinki.fi>
Maintainer: Reijo Sund <reijo.sund@helsinki.fi>
}

\keyword{ package }

\examples{
## basic usage looks like this
db <- dbConnect(SQLite(), dbname = ":memory:")
init_virtualtables(db)

## now you can use virtual tables in SQL queries.
dbDisconnect(db)
}
