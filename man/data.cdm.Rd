\name{data.cdm}

\alias{data.cdm}
\alias{data.cdm01}
\alias{data.cdm02}


\docType{data}
\title{
Several Datasets for the \pkg{CDM} Package
}

\description{
Several datasets for the \pkg{CDM} package
}

\usage{
data(data.cdm01)
data(data.cdm02)
}


\format{
\itemize{

\item Dataset \code{data.cdm01}

This dataset is a multiple choice dataset and used in the \code{\link{mcdina}}
function. The format is:

\code{List of 3} \cr
\code{ $ data    :'data.frame':} \cr
\code{  ..$ I1 : int [1:5003] 3 3 4 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ I2 : int [1:5003] 1 1 3 1 1 2 1 1 2 1 ...} \cr
\code{  ..$ I3 : int [1:5003] 4 3 2 3 2 2 2 2 1 2 ...} \cr
\code{  ..$ I4 : int [1:5003] 3 3 3 2 2 2 2 3 3 1 ...} \cr
\code{  ..$ I5 : int [1:5003] 2 2 2 3 1 1 2 3 2 1 ...} \cr
\code{  ..$ I6 : int [1:5003] 3 1 1 1 1 2 1 1 1 1 ...} \cr
\code{  ..$ I7 : int [1:5003] 1 1 2 2 1 3 1 1 1 3 ...} \cr
\code{  ..$ I8 : int [1:5003] 1 1 1 1 1 2 1 4 3 3 ...} \cr
\code{  ..$ I9 : int [1:5003] 3 2 1 1 1 1 3 3 1 3 ...} \cr
\code{  ..$ I10: int [1:5003] 2 1 2 1 1 2 2 2 2 1 ...} \cr
\code{  ..$ I11: int [1:5003] 2 2 2 2 1 2 1 2 1 1 ...} \cr
\code{  ..$ I12: int [1:5003] 1 2 1 1 2 1 1 1 1 2 ...} \cr
\code{  ..$ I13: int [1:5003] 2 1 1 1 2 1 2 2 1 1 ...} \cr
\code{  ..$ I14: int [1:5003] 1 1 1 1 1 2 1 1 2 1 ...} \cr
\code{  ..$ I15: int [1:5003] 1 2 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ I16: int [1:5003] 1 2 2 1 2 2 2 1 1 1 ...} \cr
\code{  ..$ I17: int [1:5003] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ group   : int [1:5003] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{ $ q.matrix:'data.frame':} \cr
\code{  ..$ item : int [1:52] 1 1 1 1 2 2 2 2 3 3 ...} \cr
\code{  ..$ categ: int [1:52] 1 2 3 4 1 2 3 4 1 2 ...} \cr
\code{  ..$ A1   : int [1:52] 0 1 0 1 0 1 1 1 0 0 ...} \cr
\code{  ..$ A2   : int [1:52] 0 0 1 1 0 0 0 1 0 0 ...} \cr
\code{  ..$ A3   : int [1:52] 0 0 0 0 0 0 0 0 0 0 ...} \cr

\item Dataset \code{data.cdm02}

Multiple choice dataset with a Q-matrix designed for polytomous
attributes.

\code{List of 2} \cr
\code{ $ data    :'data.frame':} \cr
\code{  ..$ I1 : int [1:3000] 3 3 4 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ I2 : int [1:3000] 1 1 3 1 1 2 1 1 2 1 ...} \cr
\code{  ..$ I3 : int [1:3000] 4 3 2 3 2 2 2 2 1 2 ...} \cr
\code{  [...] } \cr
\code{  ..$ B17: num [1:3000] 1 1 1 1 1 1 1 1 1 1 ...} \cr
\code{  ..$ B18: num [1:3000] 1 1 1 1 2 2 2 2 2 2 ...} \cr
\code{ $ q.matrix:'data.frame':} \cr
\code{  ..$ item : int [1:100] 1 1 1 1 2 2 2 2 3 3 ...} \cr
\code{  ..$ categ: int [1:100] 1 2 3 4 1 2 3 4 1 2 ...} \cr
\code{  ..$ A1   : num [1:100] 0 1 0 1 0 1 1 1 0 0 ...} \cr
\code{  ..$ A2   : num [1:100] 0 0 1 1 0 0 0 1 0 0 ...} \cr
\code{  ..$ A3   : num [1:100] 0 0 0 0 0 0 0 0 0 0 ...} \cr
\code{  ..$ B1   : num [1:100] 0 0 0 0 0 0 0 0 0 0 ...} \cr


}
  
}


%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}

%\references{
%%  ~~ possibly secondary sources and usages ~~
%}



%\examples{
%data(data.cdm01)
%## maybe str(data.cdm01) ; plot(data.cdm01) ...
%}

\keyword{datasets}
