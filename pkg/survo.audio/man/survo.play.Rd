\name{survo.play}
\alias{survo.play}
\title{
Play WAV sample
}
\description{
Provides audio support and default samples for Survo R 
}
\usage{
survo.play(sound = "default", async = TRUE, timeout = NA)
}
\arguments{
\item{sound}{Name of default sample or path for WAV-file.} 
\item{async}{Is sample played asynchronously (no wait) or synchronously (wait).}
\item{timeout}{How long time in seconds the sample will be played, NA=play whole sample.} 
}
\details{
Path to uncompressed .WAV file will play that file. There are also several samples
included with the package in the data survo.sounds (\code{\link{survo.sounds}} and those
can be used by giving just the name of the sample. 
}
\value{
There is no meaningful return value.
}
\author{
Reijo Sund
}
\seealso{
\code{\link{survo.tone}},
\code{\link{survo.sounds}}, ~~~
}
\examples{
\dontrun{
survo.play(sound="GLASS",async=FALSE,timeout=2)
survo.play("APPLAUSE")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sound }
\keyword{ audio }
\keyword{ samples }
\keyword{ WAV }
