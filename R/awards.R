awards_listing_scrape <- function(html) {

  # Each of these <div>s represents one year (=one event).
  divs <- html %>% html_elements("div.all-awards-item")

  # Each <ul> represents one award.
  ul <- html_elements(html, "div.all-awards-item > ul")

  # Each <li> represents one nomination, winning or not.
  li <- html_elements(ul, "li")

  # Event name - one per year.
  h2 <- html %>% html_elements("header.box-header > h2") %>% html_text2()

  # Replicate name, so that there is one per award. There are two types
  # of elements in the <div>s, one of them being <ul> - hence divided by 2.
  h2 <- rep(h2, xml2::xml_length(divs) / 2)

  # Replicate further, so that there is one per nomination.
  h2 <- rep(h2, xml2::xml_length(ul))

  # Award headings, one per award.
  h3 <- html_elements(html, "div.all-awards-item > h3") %>% html_text2()

  # Expand, so that there is one per nomination.
  h3 <- rep(h3, xml2::xml_length(ul))

  # Nominated may mean a person (e.g. Best director) or title (e.g. Best film).
  li_nominated <- html_element(li, "a")

  # Related may mean a title (for the Best director nomination, this would be
  # the film the person directed) or a person (for Best film, this may be its
  # directors or some other person).
  li_related <- html_element(li, "span.films > a, span.creators > a, span.description")

  tibble::tibble(

    event = h2,
    award = h3,

    nominated    = li_nominated %>% html_text2(),
    nominated_id = li_nominated %>% csfd_href(),

    related      = li_related %>% html_text2(),
    related_id   = li_related %>% csfd_href(),

    winner = li %>% html_attr("class") %>% na_replace("nominee")
  )
}
