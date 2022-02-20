#' CSFD Scraper List
#'
#' All available scrapers, indexed by name and pattern. See **Details.**
#'
#' @details
#' Indexing example: `csfd_scraper_list[["film"]]` includes
#'
#' - `.*` - these scrapers will be available on all `/film` pages
#' - `^/(prehled|recenze)` - only available on `/prehled` or `/recenze`
#' - `^/hraji` - only available on `/hraji`
#'
#' In RStudio, use `View(csfd_scraper_list)` to browse.
#'
#' Populated by `.onLoad()`.
#'
#' @format A named list with scraper functions.
#' @export
csfd_scraper_list <- NULL

# Every scraper must be registered here.
scraper_list_build <- function() {

  tv_show_episode = list(
    ".*" = list(
      summary  = film_summary_scrape,
      titles   = film_titles_scrape,
      ranks    = film_ranks_scrape,
      jobs     = film_jobs_scrape,
      releases = film_releases_scrape,
      plots    = film_plots_scrape,
      ratings  = misc_ratings_scrape
    ),
    "^/(prehled|recenze)" = list(
      reviews = misc_reviews_scrape
    ),
    "^/hraji" = list(
      cast = film_cast_scrape
    )
  )

  # TV show season has everything an episode has...
  tv_show_season <- tv_show_episode

  #...plus a brief episode listing.
  tv_show_season[[".*"]][["episodes"]] <- tv_show_season_episodes_scrape

  # Film has everything a TV show episode has...
  film <- tv_show_episode

  #...plus a fan club and a list of tags.
  film[[".*"]][["fanclub"]] <- misc_fanclub_scrape
  film[[".*"]][["tags"]] <- film_tags_scrape

  # TV show has everything a film has...
  tv_show <- film

  # ...plus a complete episode listing (includes episode ratings).
  tv_show[["^/epizody"]][["episodes"]] <- tv_show_episodes_scrape

  tvurce = list(
    ".*" = list(
      summary     = creator_summary_scrape,
      filmography = creator_filmography_scrape,
      fanclub     = misc_fanclub_scrape
    ),
    "/biografie" = list(
      bio = creator_bio_scrape
    )
  )

  uzivatel = list(
    ".*" = list(
      summary = user_summary_scrape,
      genres  = user_genres_scrape,
      types   = user_types_scrape,
      origins = user_origins_scrape,
      fanclub = misc_fanclub_scrape
    ),
    "/hodnoceni" = list(
      ratings = misc_ratings_scrape
    ),
    "/recenze" = list(
      reviews = misc_reviews_scrape
    )
  )

  kino <- list(
    "/prehled" = list(releases = kino_releases_scrape)
  )

  oceneni <- list(
    ".*" = list(listing = awards_listing_scrape)
  )

  `podrobne-vyhledavani` <- list(
    ".*" = list(
      results = search_results_scrape,
      paginator = search_paginator_scrape
    )
  )

  list(
    tv_show = tv_show,
    tv_show_season = tv_show_season,
    tv_show_episode = tv_show_episode,
    film = film,
    tvurce = tvurce,
    uzivatel = uzivatel,
    kino = kino,
    oceneni = oceneni,
    `podrobne-vyhledavani` = `podrobne-vyhledavani`
  )
}
