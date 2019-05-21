\name{correct_outliers}
\alias{correct_outliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correct irregularity in time series values.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
correct_outliers(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{vec}{input vector}

\item{window_size}{size of the bucket to traverse n by n}

\item{step_size}{the number n: Movement size of n of the window}

\item{std}{the number n: Threshold to check difference between the base and the n times standard deviation}

\item{base}{"mean" or "median"}

}
\details{
Returns a same sized vector with corrected values.}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Ibrahim Alper Koc}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
correct_outliers(c(5,2,3,2,27,2), window_size=3, step_size=2, std=2, base = "median")
}
