% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize_metadata.R
\name{visualize_metadata}
\alias{visualize_metadata}
\title{Visualize RSA-911 Metadata}
\usage{
visualize_metadata(
  data,
  option = c("general_demo", "investigate_scores", "investigate_wage",
    "investigate_employment"),
  one_window = FALSE
)
}
\arguments{
\item{data}{A cleaned metadata dataset. Apply clean_utah function first.}

\item{option}{The selected visual analysis option. The options are
"general_demo":
"investigate_scores":
"investigate_wage":
"investigate_employment":}

\item{one_window}{Whether or not the user wants the visuals to be displayed
in one plotting window, or spread across multiple plot windows. Defaults to
FALSE.}
}
\value{
The appropriate plots for the chosen visual analysis.
}
\description{
This function produces tailored visualizations for RSA and TRT scores
metadata in one step.
}
