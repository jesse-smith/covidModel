% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_delay.R
\name{estimate_delay}
\alias{estimate_delay}
\title{Estimate Reporting Delay Using a Simple Moving Average}
\usage{
estimate_delay(
  .data,
  .collection_date = "collection_date",
  .report_date = "report_date",
  pct = 0.9,
  period = 14L,
  today = Sys.Date(),
  rtn = c("last_complete", "incomplete_only", "all"),
  min_dt = as.Date("2020-04-12"),
  quiet = FALSE
)
}
\arguments{
\item{.data}{A data frame containing one incident observation per row}

\item{.collection_date}{\verb{<tidy-select>} A \code{Date} column to use as the
collection date of the observed case}

\item{.report_date}{\verb{<tidy-select>} A \code{Date} column to use as the report
date of the observed case}

\item{pct}{The quantile to use when computing the delay}

\item{period}{The number of days to average over for the rolling comparison}

\item{today}{The date to consider "today"}

\item{rtn}{What to return. By default, this is a single-row \code{tibble}
containing the last complete \code{.collection_date}; it can also return either
incomplete dates only or all dates. All return values are tibbles with
the same columns; see \code{Value} for details.}

\item{min_dt}{The minimum date to consider- set to the first reporting date
in SCHD data by default}

\item{quiet}{Should information on observations excluded from the estimation
be shown?}
}
\value{
A \code{tibble} containing one row per date and columns for
\code{.collection_date}, \code{prior_delay}, \code{delay}, and \code{incomplete} status
}
\description{
\code{estimate_delay()} estimates the time it takes for a given percentage of
samples collected on a certain date to be reported.
}
\details{
To estimate reporting delay, \code{estimate_delay()} calculates quantiles of the
delay distribution corresponding to \code{pct} for each \code{.collection_date} in the
data. If reporting is complete, these quantiles are interpretable as the
time needed for \code{pct} samples to be reported from a given date. If reporting
is incomplete, these will be biased towards the portion of the delay
distribution that is prioritized in the reporting process. In SCHD data,
cases have been mostly processed in temporal order, so this bias is
upwards (towards longer delays).

Next, quantiles are weighted by the sample size on each date, and a rolling
average is calculated with a window equal to \code{period}. This is the
continuous domain equivalent of calculating the quantile over \code{period} days.

Finally, the averages for \code{t-period} to \code{t-1} are compared to time between
\code{today} and each date \code{t}. If the average is larger than this time
difference, reporting is considered incomplete; otherwise, reporting is
considered complete.
}
