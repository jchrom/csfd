% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scraper-list.R
\docType{data}
\name{csfd_scraper_list}
\alias{csfd_scraper_list}
\title{CSFD Scraper List}
\format{
A named list with scraper functions.
}
\usage{
csfd_scraper_list
}
\description{
All available scrapers, indexed by name and pattern. See \strong{Details.}
}
\details{
Indexing example: \code{csfd_scraper_list[["film"]]} includes
\itemize{
\item \verb{.*} - these scrapers will be available on all \verb{/film} pages
\item \verb{^/(prehled|recenze)} - only available on \verb{/prehled} or \verb{/recenze}
\item \verb{^/hraji} - only available on \verb{/hraji}
}

In RStudio, use \code{View(csfd_scraper_list)} to browse.

Populated by \code{.onLoad()}.
}
\keyword{datasets}
