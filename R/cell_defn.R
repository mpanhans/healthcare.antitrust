#' Allocate Observations to Cells
#'
#' Take a dataset of hospital discharges, and assigns them to cells that
#' meet a minimum threshold cell size.
#'
#' @param data Dataset of discharges.
#' @param min_size Minimize cell size.
#' @param layers A list of lists. Each layer is a list of the variables
#'   on which observations will be allocated to cells. The layers should
#'   be ordered in decreasing refinement, so that observations not assigned
#'   to a cell meeting the minimum size threshold can be assigned by a
#'   more coarse layer.
#' @param count Name of variable which indicates the number of admissions
#'   represented by an observation. Default variable name is \code{count}.
#' @param expandLayers logical; if only a single layer is given, setting this to
#'   TRUE will expand the layer to a set of layers where each layer drops the
#'   last variable in the list of the previous layer.
#'
#' @returns A list of data frames. The first component in the list, `assigned`,
#' is the original data frame, with observations assigned to cells, and
#' excluding observations that were not assigned. The assigned cells are
#' designated by the variable \code{cell}, and the layer number in
#' which the observation is assigned to the cell is given by the variable
#' \code{cell_type}. The second component of the list, `unassigned`, is a
#' data frame with the unassigned observations.
#'
#' @details
#' If the variable \code{count} is not available, the function will assume that
#' each observation represents one admission, and a variable \code{count} will
#' be created in the output data frame to indicate this.
#'
#' When assigning observations to cells in a given layer, this function
#' only assigns observations that have not been previously assigned in a
#' finer layer. That is, the function assigns observations without
#' replacement. Assignment with replacement is currently not supported.
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
#'
#' @importFrom stats aggregate
#' @importFrom methods is
#'
#' @export

# Here could also go Roxygen comments with example tags (or links to
# vignettes) and description of output. With tags @examples and @return.

cell_defn <- function(data, min_size, layers, count = "count",
                      expandLayers = FALSE) {
  # To address check() NOTEs
  cell_tot <- NULL

  # Error checks
  if (!is(data,"data.frame")) { stop('Input needs to be a dataframe')}
  if (!is(min_size,"numeric")) { stop('Input min cell size needs to be numeric')}
  if (length(min_size)!=1) { stop('Input min cell size needs to be length 1')}
  if (!is(layers,"list")) { stop('Input layers need to be a list')}

  # Error checks in 0.1.3 (old)
  #if (!is(data,"data.frame")) {warning('Input needs to be a dataframe'); stop()}
  #if (!is(min_size,"numeric")) {warning('Input min cell size needs to be numeric'); stop()}
  #if (length(min_size)!=1) {warning('Input min cell size needs to be length 1'); stop()}
  #if (!is(layers,"list")) {warning('Input layers need to be a list'); stop()}

  ## Expand layer if single layer given and expandLayers == TRUE
  if ((length(layers)==1) & (expandLayers == TRUE)) {
    layers <- layers[[1]]

    powerlist <- vector("list", length(layers))
    nextlist <- layers
    for (i in 1:length(layers)){
      powerlist[[i]] <- nextlist
      nextlist <- nextlist[1:(length(nextlist)-1)]
    }

    layers <- powerlist
  }

  ## Allow count variable name to be missing.
  if (! count %in% names(data)) {message('Assuming one admission per row. Variable count created.'); data$count  <- 1}

  cc <- c("cell","cell_type")

  # Create ID numbers for cells based on layers of decreasing refinement
  L <- length(layers)      # Number of layers

  cell_def <- lapply(layers, function(x){unique(subset(data, select = x))})


  for (l in 1:L) {
    cell_def[[l]]$cell <- cumsum((array(1,nrow(cell_def[[l]]))))
    cell_def[[l]]$cell_type <- l
  }

  # Starting with the most refined layer, merge with discharge data.
  # Use this cell defn for cells with at least `min_size' events.
  # For cells with fewer than `min_size', go on to next level.

  cell1<-cell_def[[1]]; list1<-layers[[1]]

  # First Layer
  DD <- merge(data,cell1,by=(list1))
  tmp <- aggregate(DD[[count]],by=list(DD$cell,DD$cell_type),sum)
  names(tmp) <- c(cc,"cell_tot")
  DD <- merge(DD,tmp,by=cc)
  D0 <- subset(DD,cell_tot >= min_size)
  DD <- subset(DD,cell_tot < min_size)

  message(paste0("Layer ","1",": ",nrow(D0)," obs allocated", sep = ""))

  # Second through L Layers
  if (L > 1) {
  for (j in 2:L) {

    if (dim(DD)[1] > 0) {
    cellj<-cell_def[[j]];listj<-layers[[j]]

    DD[,cbind(cc[1],cc[2],"cell_tot")] <- list(NULL)

    DD <- merge(DD,cellj,by=(listj))
    tmp <- aggregate(DD[[count]],by=list(DD$cell,DD$cell_type),sum)
    names(tmp) <- c(cc,"cell_tot")
    DD <- merge(DD,tmp,by=cc)

    matched   <- subset(DD,cell_tot >= min_size)
    nomatched <- subset(DD,cell_tot < min_size)
    message(paste0("Layer ",j,": ",nrow(matched)," obs allocated", sep = ""))

    D0 <- rbind(D0,matched)
    DD <- nomatched
    }else{
      message(paste0("Layer ",j,": ",0," obs allocated", sep = ""))
    }

  }
  }
  message(paste0("Number of Excluded Obs: ", nrow(DD)))


  #  Create a new cell ID based on combinations of 'cell' and 'cell_type'
  cell_def <- as.data.frame(unique(cbind(D0$cell,D0$cell_type)))
  names(cell_def) <- cc
  cell_def$cell1 <- cumsum((array(1,nrow(cell_def))))
  D0 <- merge(D0,cell_def,by=cc)
  D0$cell<-D0$cell1

  D0[,cbind("cell1","cell_tot")] <- list(NULL)

  # old- needed before switch from $ to [ ]
  #names(D0)[names(D0) == "count"] <- count
  #names(DD)[names(DD) == "count"] <- count

  # Return List of Outputs
  newList <- list("assigned" = D0, "unassigned" = DD)
  return(newList)
}



