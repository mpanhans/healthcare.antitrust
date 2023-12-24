#' Diversion Ratio Calculator
#'
#' Calculates provider-level diversion ratios, once cells have been
#' defined.
#'
#' @param data Dataset of patient choices, with required variables:
#'   \code{cell}, \code{provider_id}, \code{hospital}, \code{sys_id},
#'   \code{party_ind}, \code{count}. Use other function arguments to
#'   indicate alternative variable names to the default names.
#' @param cell Name of variable specifying cell to which each observation
#'   has been allocated. Default variable name is \code{cell}. Can be
#'   created by \code{cell_defn} function.
#' @param provider_id Name of variable specifying (numeric) provider
#'   identifier. Default variable name is \code{provider_id}.
#' @param provider Name of variable specifying (string) provider name.
#'   Default variable name is \code{provider}.
#' @param sys_id Name of variable specifying (numeric) system identifier.
#'   Default variable name is \code{sys_id}.
#' @param focal_sys_id numeric; list of sys_id's specifying systems of interest
#'   for which diversions will be calculated from. For a merger, this will
#'   typically be the system identifies of the merging parties.
#' @param count Name of variable indicating the number of admissions
#'   represented by the observation. Set = 1 for every row if each
#'   observation represents one admission.
#' @param dropDegenerateCell logical; specifies how to treat cells with a
#'   100 percent within-system share. If TRUE, observations in degenerate, 100
#'   percent share cells will be ignored in the diversion ratio calculation.
#'   If FALSE, any such individuals will be assigned to the outside option,
#'   but still included in the denominator, so that the inside-option diversion
#'   will total less than 100  percent.
#' @param hosp_id deprecated; use provider_id instead.
#' @param hospital deprecated; use provider instead.
#' @param party_ind deprecated; use focal_sys_id instead.
#'
#' @returns A list with two components. The first component, `provider_level`,
#'  is a matrix giving provider-level diversions from party providers to
#'  all other providers The second object, `sys_level`, is a matrix that
#'  aggregates party providers to systems, thus giving diversions from party
#'  systems to all other providers
#'
#' @details For system-to-system diversions, set \code{provider_id} and
#'  \code{provider} equal to corresponding system-level identifiers.
#'  Diversions then reflect that patients are not allowed to divert to
#'  within-system alternative providers
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
#' out <- div_calc(D0, provider_id = "hosp_id", provider = "hospital",
#'           focal_sys_id = c(1,5))
#'
#' @importFrom stats aggregate ave
#' @importFrom methods is
#' @export



##################################################################
# Diversion Ratio Calculator
##################################################################
# Required inputs: cell, provider_id, hospital, sys_id, system, party_ind, count
# where party_ind is 1 party hospitals, zero otherwise
# and count is the number of admissions represented by the observation. =1 for all if
# each observation is one admissions

# Could be nice to pass-through system name string variable to output
# if it is supplied.

div_calc <- function(data,
                     cell = "cell",
                     provider_id = "provider_id",
                     provider = "provider",
                     sys_id = "sys_id",
                     focal_sys_id = NULL,
                     party_ind = "party_ind",
                     count = "count",
                     dropDegenerateCell = TRUE,
                     hosp_id = NA, hospital = NA) {


  # To address check() NOTEs
  #N_h <- hosp_id <- hospital <- party_sys_id <- sys_id <- NULL
  N_h <- NULL
  party_sys_id <- NULL

  # Error checks
  if (!is(data,"data.frame")) { stop('Input needs to be a dataframe')}
  if (!is(dropDegenerateCell,"logical")) { stop('Input dropDegenerateCell needs to be a logical')}

  # Temp deprecation checks for hosp_id and provider_id. Remove this eventually.
  # check if provider_id is default and hosp_id is present, then rename hosp_id
  # to provider_id and print a warning message about deprecation.
  if (! (is.na(hosp_id))) {
    provider_id <- hosp_id
    warning("hosp_id variable input is deprecated. Switch var name to provider_id.")
  }
  if (! (is.na(hospital))) {
    provider <- hospital
    warning("hospital variable input is deprecated. Switch var name to provider.")
  }

  if (!(provider_id %in% names(data)) & ("hosp_id" %in% names(data))) {
    provider_id <- "hosp_id"
    warning("hosp_id variable input is deprecated. Switch var name to provider_id.")
  }
  if (!(provider %in% names(data)) & ("hospital" %in% names(data))) {
    provider <- "hospital"
    warning("hospital variable input is deprecated. Switch var name to provider.")
  }


  # Updated var checks
  if (! cell %in% names(data)) { stop('Variable "cell" required in input dataset')}
  if (! provider_id %in% names(data)) { stop('Variable "provider_id" required in input dataset')}
  if (! provider %in% names(data)) { stop('Variable "provider" required in input dataset')}
  if (! sys_id %in% names(data)) { stop('Variable "sys_id" required in input dataset')}
  if (! count %in% names(data)) { stop('Variable "count" required in input dataset')}

  # check provider names and id's uniquely match
  check <- unique(data[c(provider_id,provider)])
  if (length(unique(check$provider_id)) != length(check$provider_id)) {warning('Error: provider_id associated with multiple provider names')}
  #if (length(unique(check$provider)) != length(check$provider)) {warning('Error: provider name associated with multiple provider_ids')}


  # focal system indicator. Later, deprecate the old party_ind method
  # IF focal_sys_id is given, use that to define party_ind
  if (!(is.null(focal_sys_id))) {
    data$party_ind <- data[[sys_id]] %in% focal_sys_id
    party_ind <- "party_ind"
  }
  # IF focal_sys_id is missing, check for party_ind variable in df.
  # if even that is missing, throw error. But error that focal_sys_id needed
  if (is.null(focal_sys_id)) {
    if (! party_ind %in% names(data)) { stop('Variable focal_sys_id required')}
  }

  data$party_sys_id <- data[[party_ind]]*data[[sys_id]]
  party_sys_list <- sort(unique(data$party_sys_id[data$party_sys_id > 0]))

  iter <- 0

  for (m in party_sys_list) {
    # Calculate cell-specific hospital diversion ratios
    y_hosp_cell = aggregate(data[[count]],by=list(data[[cell]],data[[provider_id]],data[[provider]],data$party_sys_id),sum)
    names(y_hosp_cell) <- c("cell","provider_id","provider","party_sys_id","N_h")

    y_hosp_cell$N <- ave(y_hosp_cell$N_h,y_hosp_cell$cell, FUN = sum)
    y_hosp_cell$share_h <- y_hosp_cell$N_h/y_hosp_cell$N
    y_hosp_cell$share_m <- ave(y_hosp_cell$share_h,y_hosp_cell$cell, y_hosp_cell$party_sys_id, FUN = sum)
    y_hosp_cell$share_m[y_hosp_cell$party_sys_id != m] <- 0
    y_hosp_cell$share_m <- ave(y_hosp_cell$share_m,y_hosp_cell$cell, FUN = max)

    y_hosp_cell$share_h[y_hosp_cell$party_sys_id == m] <- 0 # set share to zero for system providers

    y_hosp_cell$div <- y_hosp_cell$share_h/(1-y_hosp_cell$share_m)


    # Calculate predicted hospital-cell admissions after hospital k exclusion
    system_hosp <- sort(unique(y_hosp_cell$provider_id[y_hosp_cell$party_sys_id == m]))

    for (k in system_hosp) {
      #print(paste0("Hosp Id: ", k))  # removed for CRAN, just was status update
      iter <- iter + 1

      y_hosp_cell$N_k <- 0
      y_hosp_cell$N_k[y_hosp_cell$provider_id == k] <- y_hosp_cell$N_h[y_hosp_cell$provider_id == k]
      y_hosp_cell$N_k <- ave(y_hosp_cell$N_k,y_hosp_cell$cell, FUN = max)

      y_hosp_cell$N_h_predict <- y_hosp_cell$N_h + y_hosp_cell$N_k*y_hosp_cell$div
      y_hosp_cell$N_h_predict[y_hosp_cell$party_sys_id == m] <- 0

      # Sum across cells
      y_hosp = aggregate(data[[count]],by=list(data[[provider_id]],data[[provider]],data$party_sys_id,data[[sys_id]]),sum)
      names(y_hosp) <- c("provider_id","provider","party_sys_id","sys_id","N_h")

      y_hosp$N_k <- 0
      y_hosp$N_k[y_hosp$provider_id == k] <- y_hosp$N_h[y_hosp$provider_id == k]
      y_hosp$N_k <- max(y_hosp$N_k)

      temp <- aggregate(y_hosp_cell$N_h_predict,by=list(y_hosp_cell$provider_id),sum)
      names(temp) <- c("provider_id","N_h_predict")

      y_hosp <- merge(y_hosp,temp)

      # Calculate hospital diversion ratios - two options for denom
      if (dropDegenerateCell == FALSE) {
        y_hosp$div <- (y_hosp$N_h_predict-y_hosp$N_h)/y_hosp$N_k
      }
      if (dropDegenerateCell == TRUE) {
        y_hosp$movers <- y_hosp$N_h_predict - y_hosp$N_h
        y_hosp$N_k_alt <- sum(y_hosp$movers[y_hosp$movers>0])
        y_hosp$div <- (y_hosp$N_h_predict-y_hosp$N_h)/y_hosp$N_k_alt
      }


      y_hosp$div[y_hosp$party_sys_id == m] <- NA

      # Print flag if degenerate cells
      degenlist <- y_hosp_cell$cell[is.na(y_hosp_cell$div) & y_hosp_cell$provider_id == k]
      if (length(degenlist) > 0) {
        message("Note the following cells are degenerate: ",paste(degenlist, collapse = ", "))

        totdiv <- sum(y_hosp$div, na.rm = TRUE)
        #print(paste0("Total Diversion: ",totdiv))  # removed for CRAN, not really necessary info
      }

      if (iter == 1) {out <- subset(y_hosp, select=c(provider_id,provider,party_sys_id,sys_id,N_h))}

      out[,paste0("div_from_",k)] <- round(y_hosp$div,3)

    }

  }

  # sort for return of hospital-level diversions
  out$party_sys_id[out$party_sys_id == 0] <- NA
  out <- out[order(out$party_sys_id,out$sys_id,out$provider_id),]

  # also calculate system-level diversion
  out2 <- out
  party_sys_list <- sort(unique(out$party_sys_id[!is.na(out$party_sys_id)]))
  for (m in party_sys_list) {
    party_hosp_list <- sort(unique(out$provider_id[out$party_sys_id==m]))
    ct <- out$N_h[out2$party_sys_id==m & !is.na(out2$party_sys_id)]
    varnames <- paste("div_from_", party_hosp_list, sep="")

    out2[,paste0("div_from_sys_",m)] <- round(
      (rowSums(as.matrix(out[varnames]) %*% diag(ct, nrow = length(ct))))  / (sum(ct)),
      3)
    out2[varnames] <- NULL
  }

  # remove var party_sys_id from output. Used to sort the output matrix.
  out$party_sys_id <- NULL
  out2$party_sys_id <- NULL
  # rename column headings in output to be supplied var names
  names(out)[names(out) == "provider_id"] <- provider_id
  names(out)[names(out) == "provider"] <- provider
  names(out2)[names(out2) == "provider_id"] <- provider_id
  names(out2)[names(out2) == "provider"] <- provider

  newList <- list("provider_level" = out, "sys_level" = out2)
  return(newList)
}


