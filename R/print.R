#' @export
print.csfd_scraper <- function(x, ...) {

  cli::cli_text("{.cls {class(x)}}")

  path <- substr(x$.resp$url, 20, nchar(x$.resp$url))

  cli::cli_text("{.field path}: {path}")
  cli::cli_text("{.field html}: {.cls {class(x$html)}}")
  cli::cli_text("{.field date}: {format_dttm(x$.date)}")
  cli::cli_text("{.field body}: {object_size(x$.resp$body)}")

  fields <- grep("^(\\.|html$)", names(x), value = TRUE, invert = TRUE)

  if (length(fields)) {
    cli::cli_text("{.field {paste0('\u00a0$', fields)}}")
  }

  invisible(x)
}

object_size <- function(x) {

  bytes <- utils::object.size(x)

  units <- cut(
    bytes,
    breaks = c(0, 1024, 1024^2, 1024^3, Inf),
    labels = c("B", "KB", "MB", "GB")
  )

  format(bytes, units = as.character(units))
}

format_dttm <- function(x) {

  # Should be POSIXct, but let's have a fallback anyways.
  if (is.character(x)) return(x)

  date <- format(x, format = "%Y-%m-%d")
  time <- format(x, format = "%H:%M:%S")

  cli::format_inline("{date} {cli::col_silver(time)}")
}
