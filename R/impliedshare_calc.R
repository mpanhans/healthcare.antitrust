#' Implied Market Share Calculator
#'
#' Calculate implied market shares based on diversion ratios.
#'
#' @param M Market size.
#' @param div_from Vector of diversions from the focal good to other
#'  goods in the market, with each element between 0 and 1.
#' @param quantities Vector of quantities or revenues for each good
#'  in the market.
#' @param focal Scalar indicating the index of the focal good.
#' @param div_to Vector of diversions to the focal good from other
#'  goods in the market, with each element between 0 and 1.
#' @param reference_goods Vector indicating non-focal good(s)
#'  with diversion to focal good known, used to determine the market size.
#' @param returnM logical; default to FALSE, in which case function
#'  returns implied shares for each good in the market and corresponding
#'  membership weight for each good. If TRUE, function can be used to
#'  find value of M which best matches observed diversions to focal good.
#'
#' @returns A list with two components. The first component, `s_implied`,
#'  is a vector of implied market shares for each good in the market. The
#'  second object, `weights`, is a vector of each good's corresponding
#'  membership weight in the market.
#'
#' @details This function calculates implied market shares based on
#'  given diversion ratios.
#'
#' For more details see the example vignette by typing:
#' \code{vignette("semipar_example", package = "healthcare.antitrust")}
#'
#' @examples
#'
#' q_1 <- 40
#' q_2 <- 20
#' div_1_2 <- .3333
#' div_2_1 <- .50
#'
#' find_M <- optimize(f = impliedshare_calc, lower = 50, upper = 250,
#'    div_from=c(0,div_1_2), quantities=c(q_1,q_2), focal=1,
#'    div_to=c(0,div_2_1), reference_goods = c(2), returnM = TRUE)
#'
#' find_M
#' M_implied <- find_M$minimum
#'
#'
#' impliedshare_calc(M=M_implied,div_from=c(0,div_1_2),
#'    quantities=c(q_1,q_2),
#'    focal=1, div_to=c(0,div_2_1),reference_goods = c(2))
#'
#' @export

impliedshare_calc <- function(M,div_from, quantities, focal, div_to,
                          reference_goods, returnM = FALSE){
  ## "focal" needs to be an integer identifying the focal product.
  ## "reference_goods" indicates which products should be used to calculate
  ## market size based on having DPTS *to* the focal product.

  J <- length(quantities)

  ## Step 1: calculate a_j that matches diversions
  a_j <- div_from * (M - quantities[1]) / quantities
  a_j[focal] <- 1     ## focal product sets a_j = 1.

  if(any(a_j > 1)){message("a_j outside of [0,1]")}
  if(any(a_j < 0)){message("a_j outside of [0,1]")}

  ## Step 2: calculate implied shares
  s_implied <- (a_j * quantities)/M

  ## div_from_focal used to determine implied shares
  div_from_implied <- s_implied/(1-s_implied[focal])
  div_from_implied[focal] <- 0

  ## div_to_focal used to determine implied market size
  div_to_implied <- s_implied[focal]/(1-s_implied)
  div_to_implied[focal] <- 0

  if (returnM == TRUE) {
    out <- sum((div_to_implied[reference_goods] - div_to[reference_goods])^2)
  }
  if (returnM == FALSE) {
    out <- list("s_implied" = s_implied, "weights" = a_j)
  }
  return(out)
}


