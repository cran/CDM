\name{data.dcm}
\alias{data.dcm}
\docType{data}
\title{
Dataset from Book 'Diagnostic Measurement' of Rupp, Templin and
Henson (2010)
}


\description{
Dataset from chapter 9 of the book 'Diagnostic Measurement'
(Rupp, Templin & Henson, 2010).
}


\usage{
data(data.dcm)
}


\format{
The format of the data is a list containing the dichotomous item
response data \code{data} (10000 persons at 7 items) 
and the Q-matrix \code{q.matrix} (7 items and 3 skills):

\code{List of 2} \cr
\code{ $ data    :'data.frame':} \cr
\code{  ..$ id: int [1:10000] 1 2 3 4 5 6 7 8 9 10 ...} \cr
\code{  ..$ D1: num [1:10000] 0 0 0 0 1 0 1 0 0 1 ...} \cr
\code{  ..$ D2: num [1:10000] 0 0 0 0 0 1 1 1 0 1 ...} \cr
\code{  ..$ D3: num [1:10000] 1 0 1 0 1 1 0 0 0 1 ...} \cr
\code{  ..$ D4: num [1:10000] 0 0 1 0 0 1 1 1 0 0 ...} \cr
\code{  ..$ D5: num [1:10000] 1 0 0 0 1 1 1 0 1 0 ...} \cr
\code{  ..$ D6: num [1:10000] 0 0 0 0 1 1 1 0 0 1 ...} \cr
\code{  ..$ D7: num [1:10000] 0 0 0 0 0 1 1 0 1 1 ...} \cr
\code{ $ q.matrix: num [1:7, 1:3] 1 0 0 1 1 0 1 0 1 0 ...} \cr
\code{  ..- attr(*, "dimnames")=List of 2} \cr
\code{  .. ..$ : chr [1:7] "D1" "D2" "D3" "D4" ...} \cr
\code{  .. ..$ : chr [1:3] "skill1" "skill2" "skill3"} \cr
}

%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}

\source{
For supplementary material of the Rupp, Templin and Henson book (2010)
see \url{http://dcm.coe.uga.edu/}.

The dataset was downloaded from 
\url{http://dcm.coe.uga.edu/supplemental/chapter9.html}.
}

\references{
Rupp, A. A., Templin, J., & Henson, R. A. (2010). \emph{Diagnostic
Measurement: Theory, Methods, and Applications}. New York: The Guilford
Press.
}

\examples{
\dontrun{
data(data.dcm)

# Model 1: DINA model
mod1 <- din( data.dcm$data[,-1] , q.matrix=data.dcm$q.matrix )
summary(mod1)

# Model 2: DINO model
mod2 <- din( data.dcm$data[,-1] , q.matrix=data.dcm$q.matrix , rule="DINO")
summary(mod2)

# Model 3: log-linear model (LCDM): this model is the GDINA model with the
#    logit link function
mod3 <- gdina( data.dcm$data[,-1] , q.matrix=data.dcm$q.matrix , link="logit")
summary(mod3)

# Model 4: GDINA model with identity link function
mod4 <- gdina( data.dcm$data[,-1] , q.matrix=data.dcm$q.matrix )
summary(mod4)

# Model 5: GDINA additive model identity link function
mod5 <- gdina( data.dcm$data[,-1] , q.matrix=data.dcm$q.matrix , rule="ACDM")
summary(mod5)

# Model 6: GDINA additive model logit link function
mod6 <- gdina( data.dcm$data[,-1] , q.matrix=data.dcm$q.matrix , link="logit" , 
		rule="ACDM")
summary(mod6)

# Model 7: reduced RUM model
mod7 <- gdina( data.dcm$data[,-1] , q.matrix=data.dcm$q.matrix , rule="RRUM")
summary(mod7)

# Model 8: latent class model with 3 classes
# randomLCA package: 4 trials
library(randomLCA)
mod8a <- randomLCA( data.dcm$data[,-1], nclass= 3 , verbose=TRUE , notrials=4)
# rasch.mirtlc function in sirt package
library(sirt)
mod8b <- rasch.mirtlc( data.dcm$data[,-1] , Nclasses=3 , nstarts=4 )
summary(mod8a)
summary(mod8b)
}
}
\keyword{datasets}
