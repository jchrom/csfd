csfd_request_new <- function(method) {

  pkgs <- c("csfd", "httr2")

  user_agent <- paste(pkgs, utils::installed.packages()[pkgs, "Version"], sep = "/", collapse = " ")

  "https://www.csfd.cz" %>%
    httr2::request() %>%
    httr2::req_user_agent(user_agent) %>%
    httr2::req_throttle(15/60) %>%
    httr2::req_method(method)
}

csfd_request_perform <- function(req, quiet) {

  if (!quiet) cat_request_status(req)

  resp <- httr2::req_perform(req)

  if (!quiet) cat_request_status(resp)

  resp
}

cat_request_status <- function(x) {

  now = format(Sys.time(), format = "%H:%M:%S")
  url = substr(x$url, 20, 200)

  if (inherits(x, "httr2_request")) {
    cat_ln("{now} [ requesting ] {url}")
    return()
  }

  status <- switch(as.character(x$status_code),
    `200` = cli::col_green("[ ok ]"),
    `303` = cli::col_green("[ redirect ]"),
    cli::col_yellow("[ ", httr2::resp_status_desc(x), " ]")
  )

  cat_rn("{now} {status} {url}")

  NULL
}

cat_ln <- function(...) {
  cat(inline(...), sep = "")
}

cat_rn <- function(...) {
  cat(paste0("\r", str_pad("", cli::console_width())))
  cat("\r", inline(...), "\n", sep = "")
}

inline <- function(...) {
  cli::ansi_strtrim(cli::format_inline(..., .envir = rlang::caller_env(2)))
}
