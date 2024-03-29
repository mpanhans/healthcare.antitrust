% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wtp_calc.R
\name{wtp_calc}
\alias{wtp_calc}
\title{Willingness-to-Pay Calculator}
\usage{
wtp_calc(
  data,
  cell = "cell",
  sys_id = "sys_id",
  count = "count",
  weight = "weight",
  dropDegenerateCell = TRUE
)
}
\arguments{
\item{data}{Dataset of hospital discharges. Required variables:
\code{cell} and \code{sys_id}. Use other function arguments to
indicate alternative variable names to the default names.}

\item{cell}{Name of variable specifying cell to which each observation
has been allocated. Default variable name is \code{cell}. Can be
created by \code{cell_defn} function.}

\item{sys_id}{Name of variable specifying (numeric) system identifier.
Default variable name is \code{sys_id}.}

\item{count}{Name of variable indicating the number of admissions
represented by the observation. Set = 1 for every row if each
observation represents one admission.}

\item{weight}{Name of variable indicating designated weight of admission; =1 if
observations should be equally weighted.}

\item{dropDegenerateCell}{logical; specifies how to treat cells with a
100 percent within-system share. If TRUE, observations in degenerate,
100 percent share cells will be ignored in the WTP calculation. If FALSE,
an adjustment is made where any cells with > 99 percent share at a single
hospital have the share set to 99.0 percent.}
}
\value{
A data frame with the number of rows equal to the number of systems
in the input data frame, and the following variables: \code{sys_id} is the system
identifier, \code{WTP_s} is the system's WTP, \code{N_s} is the number of admission
for the system, and \code{WTP_s_wt} is the weighted WTP for the system
if the weight input is provided.
}
\description{
This function calculates the system-level Willingness-To-Pay.
}
\details{
This function calculates the system-level WTP. For use in a WTP
simulation exercise of the "WTP/Q Method" described in Brand and Balan
(2018) \doi{10.2139/SSRN.3153109}.
Hospital systems need to be numbered by \code{sys_id}, with a distinct
\code{sys_id} for each independent hospital.

The \code{weight} input might for example be a DRG weight for each inpatient
hospital admission.

For more details see the example vignette by typing:
\code{vignette("semipar_example", package = "healthcare.antitrust")}
}
\examples{
data(discharge_data, package = "healthcare.antitrust")

list1 <- c("drg","age","zip5")
layers <- list(list1)
th <- 15
discharge_data$count <- 1

outList <- cell_defn(discharge_data,th,layers)
D0 <- outList$assigned

out <- wtp_calc(D0)

}
