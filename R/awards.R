awards_listing_scrape <- function(html) {

  event <- html_elements(html, "div.all-awards-item")
  award <- html_elements(html, "div.all-awards-item > ul")

  nomination <- html_elements(award, "li")

  event_year_name <- html %>%
    html_elements("header.box-header > h2") %>%
    html_text2() %>%
    # Replicate event name (includes year), so that there is one line per <ul>
    # (award). The number of awards may differ between events. Each event holds
    # two types of elements in equal number: headings (<h3>) and awards (<ul>),
    # and we only care about <ul> (award). Hence divide by 2.
    rep(xml2::xml_length(event) / 2) %>%
    # One award can be associated with varying number of nominations (<li>).
    rep(xml2::xml_length(award))

  event_year <- str_extract(event_year_name, "^[0-9]+") %>% as.integer()
  event_name <- str_remove(event_year_name, "^[0-9]+ . ")

  award_name <- html %>%
    html_elements("div.all-awards-item > h3") %>%
    html_text2() %>%
    # Replicate, so that there is one per nomination. One award can hold multiple
    # nominations (variable).
    rep(xml2::xml_length(award))

  # "Nominated" may mean a person (e.g. Best director) or title (e.g. Best film).
  nominated <- html_element(nomination, "a")

  # "Related" may mean a title (for Best director, this would be their film)
  # or a person (for Best film, its director or other profession).
  related <- html_element(nomination, "span.films > a, span.creators > a, span.description")

  tibble::tibble(
    year  = event_year,
    event = event_name,
    award = award_name,

    nominated_name = nominated %>% html_text2(),
    nominated_id   = nominated %>%
      html_attr("href") %>%
      regx_extract_id(),

    related_name = related %>% html_text2(),
    related_id   = related %>%
      html_attr("href") %>%
      regx_extract_id(),

    status = nomination %>%
      html_attr("class") %>%
      ifelse(is.na(.), "nominated", .)
  )
}
