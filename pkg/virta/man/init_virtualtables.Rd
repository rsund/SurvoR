\name{init_virtualtables}
\alias{init_virtualtables}

\title{
Initialize SQLite extension virtual tables on a SQLite DB connection
}
\description{
This function loads extension virtual tables to make them available to an
RSQLite database connection.  The databse connection needs to have been
created by passing \code{loadable.extensions = TRUE} to \code{dbConnect}
(this is the default as of RSQLite 0.9-1).
}
\usage{
init_virtualtables(db)
}

\arguments{
  \item{db}{
    An RSQLite database connection as returned by \code{dbConnect}.
}
}
\value{
  TRUE if the virtual tables were successfully loaded.
}
\references{
 For details on loadable extensions in SQLite, see
 \url{http://www.sqlite.org/cvstrac/wiki?p=LoadableExtensions}.
 
 For details on virtual tables in SQLite, see
 \url{http://www.sqlite.org/cvstrac/wiki?p=VirtualTables}.

}
\author{
Reijo Sund
}

\examples{
db <- dbConnect(SQLite(), dbname = ":memory:",
                loadable.extensions = TRUE)

init_virtualtables(db)

dbSendQuery(db, "create virtual table t1 using test")
dbGetQuery(db,"select * from t1")

dbDisconnect(db)
}
