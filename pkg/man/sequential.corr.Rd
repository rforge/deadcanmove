\name{sequential.corr}
\alias{sequential.corr}
\title{
Sequential correlation
}
\description{
This functiopn calculates the correlation between the hotspots of each submatrix and those of the baseline (sub)matrix of the corresponding group.
}
\usage{
sequential.corr(hotspots.list, hotspots.thresholds, comp.method = "Phi", baseline.interval = 1, messages = "TRUE")
}
\arguments{
  \item{hotspots.list}{a list of hotspot tables resulting from the sequential.hotspots function}
  \item{hotspots.thresholds}{a matrix of hotspots thresholds (element 2 of the results of the hotspot.numbers function)}
  \item{comp.method}{characer value indicating the correlation coefficient to use; type \code{binary.comp.methods()} for available options}
  \item{baseline.interval}{the sampling interval with which to correlate all the other sampling intervals for each group}
  \item{messages}{logical value indicating whether to display messages}
}
\details{
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
}
\author{
%%  ~~who you are~~
}
\note{
}
\seealso{
\code{\link{binary.comparison}}, \code{\link{binary.comp.methods}}
}
\examples{
data(roadkills)

submats <- sequential.submatrix(dataset = roadkills, sampl.columns = 3:ncol(roadkills), sampl.intervals = c(1, 2, 3), group.column = "group", include.all.together = TRUE, remove.zeros = TRUE, keep.nonsampl.columns = TRUE)

hsl <- sequential.hotspots(dataset = roadkills, submats = submats, region.column = "site", first.subsampl.col = 3, confidence = 0.95)

hsn <- hotspot.numbers(hotspots.list = hsl, sampl.intervals = 1:3, groups = as.character(unique(roadkills$group)), include.all.together = TRUE, min.total.events = 80, min.hotspot.threshold = 2)

seqcorr <- sequential.corr(hotspots.list = hsl, hotspots.thresholds = hsn$HS.threshold, comp.method = "Phi", baseline.interval = 1, messages = "TRUE")

seqcorr
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line