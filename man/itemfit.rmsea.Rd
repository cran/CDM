\name{itemfit.rmsea}
\alias{itemfit.rmsea}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
RMSEA Item Fit
}
\description{
This function estimates a chi squared based measure of item fit
in cognitive diagnosis models as implemented in mdltm (von Davier, 2005;
cited in Kunina-Habenicht, Rupp & Wilhelm, 2009).
}
\usage{
itemfit.rmsea(n.ik, pi.k, probs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n.ik}{
An array of four dimensions: Classes x Items x Categories x Groups
}
  \item{pi.k}{
An array of two dimensions: Classes x Groups
}
  \item{probs}{
An array of four dimensions: Classes x Items x Categories x Groups
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector with RMSEA item fit statistics
}
\references{
Kunina-Habenicht, O., Rupp, A.A., & Wilhelm, O. (2009). 
A practical illustration of multidimensional diagnostic skills profiling: 
Comparing results from confirmatory factor analysis and diagnostic classification models. 
\emph{Studies in Educational Evaluation}, \bold{35}, 64--70.

von Davier, M. (2005). \emph{A general diagnostic model applied to language 
testing data}, Research Report RR-05-16. ETS, Princeton, NJ: ETS.
}
\author{
Alexander Robitzsch
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
This function is used in \code{\link{din}}, \code{\link{gdina}} and
\code{\link{gdm}}.
}
%\examples{
%%%
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Item fit}
\keyword{RMSEA}
