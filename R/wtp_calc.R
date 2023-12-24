#' Willingness-to-Pay Calculator
#'
#' This function calculates the system-level Willingness-To-Pay.
#'
#' @param data Dataset of hospital discharges. Required variables:
#'   \code{cell} and \code{sys_id}. Use other function arguments to
#'   indicate alternative variable names to the default names.
#' @param cell Name of variable specifying cell to which each observation
#'   has been allocated. Default variable name is \code{cell}. Can be
#'   created by \code{cell_defn} function.
#' @param sys_id Name of variable specifying (numeric) system identifier.
#'   Default variable name is \code{sys_id}.
#' @param count Name of variable indicating the number of admissions
#'   represented by the observation. Set = 1 for every row if each
#'   observation represents one admission.
#' @param weight Name of variable indicating designated weight of admission; =1 if
#'   observations should be equally weighted.
#' @param dropDegenerateCell logical; specifies how to treat cells with a
#'   100 percent within-system share. If TRUE, observations in degenerate,
#'   100 percent share cells will be ignored in the WTP calculation. If FALSE,
#'   an adjustment is made where any cells with > 99 percent share at a single
#'   hospital have the share set to 99.0 percent.
#'
#' @returns A data frame with the number of rows equal to the number of systems
#' in the input data frame, and the following variables: `sys_id` is the system
#'  identifier, `WTP_s` is the system's WTP, `N_s` is the number of admission
#'  for the system, and `WTP_s_wt` is the weighted WTP for the system
#'  if the weight input is provided.
#'
#' @details This function calculates the system-level WTP. For use in a WTP
#'  simulation exercise of the "WTP/Q Method" described in Brand and Balan
#'  (2018) \doi{10.2139/SSRN.3153109}.
#'  Hospital systems need to be numbered by \code{sys_id}, with a distinct
#'  \code{sys_id} for each independent hospital.
#'
#'  The `weight` input might for example be a DRG weight for each inpatient
#'  hospital admission.
#'
#' For more details see the example vignette by typing:
#' \code{vignette("semipar_example", package = "healthcare.antitrust")}
#'
#' @examples
#' data(discharge_data, package = "healthcare.antitrust")
#'
#' list1 <- c("drg","age","zip5")
#' layers <- list(list1)
#' th <- 15
#' discharge_data$count <- 1
#'
#' outList <- cell_defn(discharge_data,th,layers)
#' D0 <- outList$assigned
#'
#' out <- wtp_calc(D0)
#'
#' @importFrom stats aggregate ave
#' @export



##################################################################
# System WTP Calculator
##################################################################

# This function calculates the system-level WTP.
# MAYBE add system id to the output?

wtp_calc <- function(data,
                     cell = "cell",
                     sys_id = "sys_id",
                     count = "count",
                     weight = "weight",
                     dropDegenerateCell = TRUE) {

  ## Error checks
  if (! cell %in% names(data)) { stop('Variable "cell" required in input dataset')}
  if (! sys_id %in% names(data)) { stop('Variable "sys_id" required in input dataset')}

  ## allow weight to be named or, if missing, set = 1.
  if (! weight %in% names(data)) { data$weight  <- 1}

  data$totalweight <- data[[weight]]*data[[count]]  # in case some obs are aggregated admissions

  y_cell <- aggregate(list(N_s=data[[count]], wt = data$totalweight),by=list(cell=data[[cell]], sys_id=data[[sys_id]]),sum)

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

  y <- aggregate(list(WTP_s=y_cell$WTP_s, N_s=y_cell$N_s, WTP_s_wt=y_cell$WTP_s_wt),by=list(sys_id=y_cell$sys_id),sum)

  return(y)
}


