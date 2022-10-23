.onLoad <- function(libname, pkgname) {

  # Every scraper must be registered here.
  #
  # Items in these lists must be regular expressions, matching URL paths
  # of the incoming HTML pages.

  episode = list(
    ".*" = list(
      details  = film_details_scrape,
      titles   = film_titles_scrape,
      ranks    = film_ranks_scrape,
      jobs     = film_jobs_scrape,
      releases = film_releases_scrape,
      plots    = film_plots_scrape,
      ratings  = misc_ratings_scrape
    ),
    "/(prehled|recenze)" = list(
      reviews = misc_reviews_scrape
    ),
    "/hraji" = list(
      cast = film_cast_scrape
    )
  )

  season <- list(
    ".*" = list(
      details  = film_details_scrape,
      ranks    = film_ranks_scrape,
      jobs     = film_jobs_scrape,
      releases = film_releases_scrape,
      ratings  = misc_ratings_scrape,
      episodes = season_episodes_scrape
    ),
    "/(prehled|recenze)" = list(
      reviews = misc_reviews_scrape
    ),
    "/hraji" = list(
      cast = film_cast_scrape
    )
  )

  film <- list(
    ".*" = list(
      details  = film_details_scrape,
      titles   = film_titles_scrape,
      ranks    = film_ranks_scrape,
      jobs     = film_jobs_scrape,
      releases = film_releases_scrape,
      plots    = film_plots_scrape,
      tags     = film_tags_scrape,
      ratings  = misc_ratings_scrape,
      fanclub  = misc_fanclub_scrape
    ),
    "/(prehled|recenze)" = list(
      reviews = misc_reviews_scrape
    ),
    "/hraji" = list(
      cast = film_cast_scrape
    )
  )

  show <- list(
    ".*" = list(
      details  = film_details_scrape,
      titles   = film_titles_scrape,
      ranks    = film_ranks_scrape,
      jobs     = film_jobs_scrape,
      releases = film_releases_scrape,
      plots    = film_plots_scrape,
      tags     = film_tags_scrape,
      ratings  = misc_ratings_scrape,
      fanclub  = misc_fanclub_scrape
    ),
    "/(prehled|recenze)" = list(
      reviews = misc_reviews_scrape
    ),
    "/epizody" = list(
      episodes = show_episodes_scrape
    ),
    "/hraji" = list(
      cast = film_cast_scrape
    )
  )

  tvurce = list(
    ".*" = list(
      details     = creator_details_scrape,
      filmography = creator_filmography_scrape,
      fanclub     = misc_fanclub_scrape
    ),
    "/biografie" = list(
      bio = creator_bio_scrape
    )
  )

  uzivatel = list(
    ".*" = list(
      details = user_details_scrape,
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

  vyhledavani <- list(
    ".*" = list(
      paginator = search_paginator_scrape
    ),
    "vyhledavani(?!/tvurci)" = list(
      results = search_titles_scrape
    ),
    "vyhledavani/tvurci" = list(
      results = search_creators_scrape
    )
  )

  zebricky <- list(
    "/(serialy|filmy)" = list(
      titles = leaderboard_titles_scrape
    ),
    "/(herci|scenariste)" = list(
      creators = leaderboard_fans_scrape
    ),
    "/reziseri" = list(
      directors_by_fanclub_size = leaderboard_fans_scrape,
      directors_by_movie_rating = leaderboard_ratings_scrape
    ),
    "/vlastni" = list(
      results = leaderboard_titles_scrape,
      paginator = search_paginator_scrape
    )
  )

  scraper_list <- list(
    `^/tvurce`   = tvurce,
    `^/uzivatel` = uzivatel,
    `^/film`     = film,
    `^/show`     = show,
    `^/season`   = season,
    `^/episode`  = episode,
    `^/kino`     = kino,
    `^/oceneni`  = oceneni,
    `^/podrobne` = vyhledavani,
    `^/zebricky` = zebricky
  )

  assign(
    x     = "csfd_scraper_list",
    value = structure(scraper_list, class = "csfd_scraper_list"),
    envir = getNamespace("csfd")
  )

  invisible()
}
