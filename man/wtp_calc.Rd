% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wtp_calc.R
\name{wtp_calc}
\alias{wtp_calc}
\title{Willingness-to-Pay Calculator}
\usage{
wtp_calc(
  D,
  cell = "cell",
  sys_id = "sys_id",
  party_ind = "party_ind",
  adm = "adm",
  weight = "weight",
  dropDegenerateCell = TRUE
)
}
\arguments{
\item{D}{Dataset of hospital discharges. Required variables:
\code{cell}, \code{sys_id}, \code{party_ind}, \code{adm}, and
\code{weight}. Use other function arguments to
indicate alternative variable names to the defaul names.}

\item{cell}{Name of variable specifying cell to which each observation
has been allocated. Default variable name is \code{cell}. Can be
created by \code{cell_defn} function.}

\item{sys_id}{Name of variable specifying (numeric) system identifier.
Default variable name is \code{sys_id}.}

\item{party_ind}{Name of indicator variable for whether hospital is a
party from which diversions should be calculated. Default variable
name is \code{party_ind}.}

\item{adm}{Name of variable indicating the number of admissions
represented by the observation. Set = 1 for every row if each
observation represents one admission.}

\item{weight}{The designated DRG weight of admission; =1 if
observations should be equally weighted.}

\item{dropDegenerateCell}{logical; specifies how to treat cells with a
100\% within-system share. If TRUE, observations in degenerate, 100\% share
cells will be ignored in the WTP calculation. If FALSE, an adjustment
is made where any cells with > 99\% share at a single hospital have the
share set to 99.0\%.}
}
\description{
This function calculates the system-level Willingness-To-Pay.
}
\details{
This function calculates the system-level WTP. For use in a WTP
 simulation exercise of the "WTP/Q Method" described in Brand and Balan
 (2018) <https://doi.org/10.2139/SSRN.3153109>.
 Hospital systems need to be numbered by \code{sys_id}, with a distinct
 \code{sys_id} for each independent hospital.

 The function outputs both weighted and unweighted WTP by system.
 For example, is it common to weight each observation by DRG weight.
 For only unweighted results, weight should be = 1 for all observations,
 then the two measures will be equivalent.

For more details see the example vignette by typing:
\code{vignette("semipar_example", package = "healthcare.antitrust")}
}
