\name{survo.tone}
\alias{survo.tone}
\title{
Play pure tone
}
\description{
Provides pure tone playing for Survo R 
}
\usage{
survo.tone(tonefreq=420, duration = 0.2, sampfreq=22050)
}
\arguments{
\item{tonefreq}{Tone to play in Hz.} 
\item{duration}{Length of tone in seconds.}
\item{sampfreq}{Sampling frequency to be used.} 
}
\details{
Plays whole tone synchronously.
}
\value{
There is no return value.
}
\author{
Reijo Sund
}
\seealso{
\code{\link{survo.play}}, ~~~
}
\examples{
\dontrun{
survo.tone()
survo.tone(1500,2,44100)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sound }
\keyword{ audio }
\keyword{ samples }
\keyword{ WAV }
