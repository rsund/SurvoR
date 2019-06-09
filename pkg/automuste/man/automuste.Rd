\name{automuste}
\alias{automuste}

\title{
Autoupdater for Survo R
}
\description{
This function autoupdates Survo R (package muste and related packages) and autostarts Survo Editor.
}
\usage{
automuste(kysy=FALSE,curl=contrib.url(repos="http://www.survo.fi", type = "binary"))
}

\arguments{
  \item{kysy}{
    "Ask" parameter for new.packages and update.packages (FALSE/TRUE/"graphical")
}
 \item{curl}{
    "Contriburl" parameter for package function.
}
}
\author{
Reijo Sund
}
