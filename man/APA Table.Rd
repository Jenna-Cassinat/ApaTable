\name{LMApaTable}
\alias{LMApaTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LM APA Table
}
\description{
This will take your lm output and put it into a semi-correct APA table with the coefficient, standard error and the P-value.
}
\usage{
LMApaTable(model, Bold = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
This is a model return from lm.
}
  \item{Bold}{
This is an option for if you would like to bold statistically significant coefficients from your model.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This will return a markdown table that when knit will look like an APA table (sort of).
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
