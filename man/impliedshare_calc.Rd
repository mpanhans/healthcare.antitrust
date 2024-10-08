% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impliedshare_calc.R
\name{impliedshare_calc}
\alias{impliedshare_calc}
\title{Implied Market Share Calculator}
\usage{
impliedshare_calc(
  M,
  div_from,
  quantities,
  focal,
  div_to,
  reference_goods,
  returnM = FALSE
)
}
\arguments{
\item{M}{Market size.}

\item{div_from}{Vector of diversions from the focal good to other
goods in the market, with each element between 0 and 1.}

\item{quantities}{Vector of quantities or revenues for each good
in the market.}

\item{focal}{Scalar indicating the index of the focal good.}

\item{div_to}{Vector of diversions to the focal good from other
goods in the market, with each element between 0 and 1.}

\item{reference_goods}{Vector indicating non-focal good(s)
with diversion to focal good known, used to determine the market size.}

\item{returnM}{logical; default to FALSE, in which case function
returns implied shares for each good in the market and corresponding
membership weight for each good. If TRUE, function can be used to
find value of M which best matches observed diversions to focal good.}
}
\value{
A list with two components. The first component, \code{s_implied},
is a vector of implied market shares for each good in the market. The
second object, \code{weights}, is a vector of each good's corresponding
membership weight in the market.
}
\description{
Calculate implied market shares based on diversion ratios.
}
\details{
This function calculates implied market shares based on
given diversion ratios.

For more details see the example vignette by typing:
\code{vignette("semipar_example", package = "healthcare.antitrust")}
}
\examples{

q_1 <- 40
q_2 <- 20
div_1_2 <- .3333
div_2_1 <- .50

find_M <- optimize(f = impliedshare_calc, lower = 50, upper = 250,
   div_from=c(0,div_1_2), quantities=c(q_1,q_2), focal=1,
   div_to=c(0,div_2_1), reference_goods = c(2), returnM = TRUE)

find_M
M_implied <- find_M$minimum


impliedshare_calc(M=M_implied,div_from=c(0,div_1_2),
   quantities=c(q_1,q_2),
   focal=1, div_to=c(0,div_2_1),reference_goods = c(2))

}
