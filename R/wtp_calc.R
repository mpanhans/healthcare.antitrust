#' Willingness-to-Pay Calculator
#'
#' This function calculates the system-level Willingness-To-Pay.
#'
#' @param data Dataset of hospital discharges. Required variables:
#'   \code{cell}, \code{sys_id}, \code{party_ind}, \code{count}, and
#'   \code{weight}. Use other function arguments to
#'   indicate alternative variable names to the defaul names.
#' @param cell Name of variable specifying cell to which each observation
#'   has been allocated. Default variable name is \code{cell}. Can be
#'   created by \code{cell_defn} function.
#' @param sys_id Name of variable specifying (numeric) system identifier.
#'   Default variable name is \code{sys_id}.
#' @param party_ind Name of indicator variable for whether hospital is a
#'   party from which diversions should be calculated. Default variable
#'   name is \code{party_ind}.
#' @param count Name of variable indicating the number of admissions
#'   represented by the observation. Set = 1 for every row if each
#'   observation represents one admission.
#' @param weight The designated DRG weight of admission; =1 if
#'   observations should be equally weighted.
#' @param dropDegenerateCell logical; specifies how to treat cells with a
#' 100\% within-system share. If TRUE, observations in degenerate, 100\% share
#' cells will be ignored in the WTP calculation. If FALSE, an adjustment
#' is made where any cells with > 99\% share at a single hospital have the
#' share set to 99.0\%.
#'
#' @details This function calculates the system-level WTP. For use in a WTP
#'  simulation exercise of the "WTP/Q Method" described in Brand and Balan
#'  (2018) <https://doi.org/10.2139/SSRN.3153109>.
#'  Hospital systems need to be numbered by \code{sys_id}, with a distinct
#'  \code{sys_id} for each independent hospital.
#'
#'  The function outputs both weighted and unweighted WTP by system.
#'  For example, is it common to weight each observation by DRG weight.
#'  For only unweighted results, weight should be = 1 for all observations,
#'  then the two measures will be equivalent.
#'
#' For more details see the example vignette by typing:
#' \code{vignette("semipar_example", package = "healthcare.antitrust")}
#' @importFrom stats aggregate ave
#' @export



##################################################################
# System WTP Calculator
##################################################################

# This function calculates the system-level WTP.
# MAYBE add system id to the output?
# Also I think party_ind is not really used, remove as input?

wtp_calc <- function(data,
                     cell = "cell",
                     sys_id = "sys_id",
                     party_ind = "party_ind",
                     count = "count",
                     weight = "weight",
                     dropDegenerateCell = TRUE) {

  ## allow for generic variable names. old. Now [] instead of $
  #names(data)[names(data) == cell] <- "cell"
  #names(data)[names(data) == sys_id] <- "sys_id"
  #names(data)[names(data) == party_ind] <- "party_ind"
  #names(data)[names(data) == count] <- "count"

  ## allow weight to be named or, if missing, set = 1.
  ## Next two lines were old version, now using [] instead of $
  #names(data)[names(data) == weight] <- "weight"
  #if (!"weight" %in% names(data)) { data$weight  <- 1}
  if (! weight %in% names(data)) { data$weight  <- 1}

  #data$totalweight <- data$weight*data$count  # in case some obs are aggregated admissions
  data$totalweight <- data[[weight]]*data[[count]]  # in case some obs are aggregated admissions

  y_cell <- aggregate(list(N_s=data[[count]], wt = data$totalweight),by=list(cell=data[[cell]], sys_id=data[[sys_id]],party=data[[party_ind]]),sum)

  y_cell <- y_cell[order(y_cell$cell,y_cell$sys_id),]

  y_cell$N <- ave(y_cell$N_s,y_cell$cell, FUN = sum)
  y_cell$share_s <- y_cell$N_s/y_cell$N

  y_cell$wt <- ave(y_cell$wt,y_cell$cell, FUN = sum)
  y_cell$wt <- y_cell$wt/y_cell$N

  # Drop cells with only one hospital visited or do adjustment
  if (dropDegenerateCell == TRUE) {
    y_cell <- subset(y_cell,y_cell$share_s < 1)
  }
  if (dropDegenerateCell == FALSE) {
    y_cell$share_s[y_cell$share_s > .99] <- .99
  }


  y_cell$WTP_s <- log(1/(1-y_cell$share_s))*y_cell$N
  y_cell$WTP_s_wt <- log(1/(1-y_cell$share_s))*y_cell$N*y_cell$wt

  y <- aggregate(list(WTP_s=y_cell$WTP_s, N_s=y_cell$N_s, WTP_s_wt=y_cell$WTP_s_wt),by=list(party=y_cell$party,sys_id=y_cell$sys_id),sum)

  # Revert variable names
  #names(y)[names(y) == "cell"] <- cell
  #names(y)[names(y) == "sys_id"] <- sys_id
  #names(y)[names(y) == "party_ind"] <- party_ind
  #names(y)[names(y) == "count"] <- count

  return(y)
}


