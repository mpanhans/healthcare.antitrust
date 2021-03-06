#' Allocate Observations to Cells
#'
#' Take a dataset of hospital discharges, and assigns them to cells that
#' meet a minimum threshold cell size.
#'
#' @param D Dataset of discharges.
#' @param s_min Minimize cell size.
#' @param layers A list of lists. Each layer is a list of the variables
#'   on which observations will be allocated to cells. The layers should
#'   be ordered in decreasing refinement, so that observations not assigned
#'   to a cell meeting the minimum size threshold can be assigned by a
#'   more coase layer.
#' @param adm Name of variable which indicates the number of admissions
#'   represented by an observation. Default variable name is \code{adm}.
#'
#' @details The output is a list of datasets. The first item in the list
#' is the original dataset, with observations assigned to cells. The
#' cells are designated by the variable \code{cell}, and the layer number in
#' which the observation is assigned to the cell is given by the variable
#' \code{cell_type}. The second item in the output list is a dataset of the
#' unassigned observations.
#'
#' If the variable \code{adm} is not available, the function will assume that
#' each observation represents one admission, and a variable \code{adm} will
#' be created in the output dataset to indicate this.
#'
#' When assigning observations to cells in a given layer, this function
#' only assigns observations that have not been previously assigned in a
#' finer layer. That is, the function assigns observations without
#' replacement. Assignment with replacement is currently not supported.
#'
#' For more details see the example vignette by typing:
#' \code{vignette("semipar_example", package = "healthcare.antitrust")}
#'
#' @importFrom stats aggregate
#'
#' @export

# Here could also go Roxygen comments with example tags (or links to
# vignettes) and description of output. With tags @examples and @return.

cell_defn <- function(D, s_min, layers, adm = "adm") {
  # To address check() NOTEs
  cell_tot <- NULL

  # Error checks
  if (!is(D,"data.frame")) {warning('Input needs to be a dataframe'); stop()}
  if (!is(s_min,"numeric")) {warning('Input min cell size needs to be numeric'); stop()}
  if (length(s_min)!=1) {warning('Input min cell size needs to be length 1'); stop()}
  if (!is(layers,"list")) {warning('Input layers need to be a list'); stop()}

  ## Allow adm variable name to be missing.
  #names(D)[names(D) == adm] <- "adm"
  #if (!"adm" %in% names(D)) {message('Assuming one admission per row. Variable adm created.'); D$adm  <- 1}
  ## above two lines were old version, now using [] instead of $
  if (! adm %in% names(D)) {message('Assuming one admission per row. Variable adm created.'); D$adm  <- 1}

  cc <- c("cell","cell_type")

  # Create ID numbers for cells based on layers of decreasing refinement
  L <- length(layers)      # Number of layers

  cell_def <- lapply(layers, function(x){unique(subset(D, select = x))})


  for (l in 1:L) {
    cell_def[[l]]$cell <- cumsum((array(1,nrow(cell_def[[l]]))))
    cell_def[[l]]$cell_type <- l
  }

  # Starting with the most refined layer, merge with discharge data.
  # Use this cell defn for cells with at least `s_min' events.
  # For cells with fewer than `s_min', go on to next level.

  cell1<-cell_def[[1]]; list1<-layers[[1]]

  # First Layer
  DD <- merge(D,cell1,by=(list1))
  tmp <- aggregate(DD[[adm]],by=list(DD$cell,DD$cell_type),sum)
  names(tmp) <- c(cc,"cell_tot")
  DD <- merge(DD,tmp,by=cc)
  D0 <- subset(DD,cell_tot >= s_min)
  DD <- subset(DD,cell_tot < s_min)

  print(paste0("Layer ","1",": ",nrow(D0)," obs allocated", sep = ""))

  # Second through L Layers
  if (L > 1) {
  for (j in 2:L) {

    if (dim(DD)[1] > 0) {
    cellj<-cell_def[[j]];listj<-layers[[j]]

    DD[,cbind(cc[1],cc[2],"cell_tot")] <- list(NULL)

    DD <- merge(DD,cellj,by=(listj))
    tmp <- aggregate(DD[[adm]],by=list(DD$cell,DD$cell_type),sum)
    names(tmp) <- c(cc,"cell_tot")
    DD <- merge(DD,tmp,by=cc)

    matched   <- subset(DD,cell_tot >= s_min)
    nomatched <- subset(DD,cell_tot < s_min)
    print(paste0("Layer ",j,": ",nrow(matched)," obs allocated", sep = ""))

    D0 <- rbind(D0,matched)
    DD <- nomatched
    }else{
    print(paste0("Layer ",j,": ",0," obs allocated", sep = ""))
    }

  }
  }
  print(paste0("Number of Excluded Obs: ", nrow(DD)))


  #  Create a new cell ID based on combinations of 'cell' and 'cell_type'
  cell_def <- as.data.frame(unique(cbind(D0$cell,D0$cell_type)))
  names(cell_def) <- cc
  cell_def$cell1 <- cumsum((array(1,nrow(cell_def))))
  D0 <- merge(D0,cell_def,by=cc)
  D0$cell<-D0$cell1

  D0[,cbind("cell1","cell_tot")] <- list(NULL)

  # old- needed before switch from $ to [ ]
  #names(D0)[names(D0) == "adm"] <- adm
  #names(DD)[names(DD) == "adm"] <- adm

  # Return List of Outputs
  newList <- list("dataset" = D0, "unassigned" = DD)
  return(newList)
}



