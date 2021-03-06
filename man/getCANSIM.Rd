% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/getCANSIM.r
\name{getCANSIM}
\alias{getCANSIM}
\title{getCANSIM}
\usage{
getCANSIM(cansimTableNumber='', showLabels = TRUE, raw = FALSE, lang = 'eng')
}
\arguments{
\item{cansimTableNumber}{- the table number we wish to retrieve from CANSIM.}

\item{showLabels}{- show the Statistics Canada labels after finishing extracting and converting the table, TRUE by default.}

\item{raw}{- download the CANSIM table as-is, skipping all processing, FALSE by default.}

\item{lang}{- language for data labels ('eng' for English or 'fra' for French), 'eng' by default.}
}
\value{
data frame containing CANSIM table.
}
\description{
Extracts a complete CANSIM (Statistics Canada) data table
and converts it into a readily usable panel (wide) format.
}
\details{
Geographic variables are renamed i, time variables are renamed t,
and all the other variables are renamed with a generic V1, V2, ..., Vn.
The generic variables keep the full Statistics Canada description by using a label.
}
\examples{
getCANSIM("12-10-0005")
getCANSIM("12-10-0005", lang = 'fra')
}
