
<!-- README.md is generated from README.Rmd. Please edit that file -->

# csfd

<!-- badges: start -->

[![R-CMD-check](https://github.com/jchrom/csfd/workflows/R-CMD-check/badge.svg)](https://github.com/jchrom/csfd/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Smyslem balíčku csfd je umožnit novinářům a akademickým pracovníkům
skrejpování veřejně přístupných dat z [Česko-Slovenské filmové databáze
(ČSFD)](https://www.csfd.cz). Balíček není oficiálním produktem ČSFD a
není určen ke komerčnímu využití. Pokud někdy v budoucnu ČSFD uvede do
provozu standardní webovou API (po vzoru IMDB), bude balíček deprekován.

## Instalace

``` r
# install.packages("devtools")
devtools::install_github("jchrom/csfd")
```

## Frekvence stahování

Frekvence stahování je omezena na 15 požadavků za minutu, aby se
zamezilo přetěžování serveru neurvalým masovým skrejpováním.

## Příklad

Příkazem níže získáme stránku třetí série seriálu *Star Trek: Nová
generace.* Adresu
<https://www.csfd.cz/film/69432-star-trek-nova-generace/483364-serie-3/prehled/>
můžeme sice zadat celou, ale postačí i část s typem díla a číselný
identifikátor (cesty audiovizuálních děl začínají na ČSFD vždy `/film`,
bez ohledu na typ díla):

``` r
library(csfd)
tng <- csfd_fetch("/film/483364", quiet = TRUE)
```

Výsledkem je objekt `<csfds_scraper>`, který obsahuje zdrojový HTML kód
a jednu nebo několik funkcí, určených k seškrabání dat. V příkladu níže
jsou scrapery označeny symbolem `$`.

``` r
tng
#> <csfd_scraper>
#> path: /film/69432-star-trek-nova-generace/483364-serie-3/prehled/
#> html: <xml_document/xml_node>
#> date: 2022-10-14 08:38:35
#> body: 84.3 Kb
#>  $reviews,  $releases,  $summary,  $plots,  $jobs,  $titles,  $episodes,
#>  $ratings, and  $ranks
```

Vybraný scraper zavoláme stejně, jako když vracíme hodnotu při běžném
indexování, například použitím `$`:

``` r
tng$summary
#> # A tibble: 1 × 11
#>   title    id    genre poster parts origin relea…¹ ended time_…² short…³ longe…⁴
#>   <chr>    <chr> <chr> <chr>  <int> <chr>    <int> <int>   <dbl>   <int>   <int>
#> 1 Star Tr… film… Sci-… https…    26 USA       1989  1990    1219      45      60
#> # … with abbreviated variable names ¹​released, ²​time_min, ³​shortest_min,
#> #   ⁴​longest_min
```

`<csfds_scraper>` pozná, o jaký typ stránky se jedná, takže máte vždy k
dispozici ty správné scrapery. Občas se stane, že má vrácená tabulka
nula řádků, pokud určitá data na stránce chybí. Například tag cloud bývá
na stránkách filmů/seriálů, ale pokud nikdo dané dílo neotagoval, data
budou logicky chybět.

## Další příklady

Stažení stránky se seznamem epizod:

``` r
# Kmpletni URL.
csfd_fetch("https://www.csfd.cz/film/69432-star-trek-nova-generace/epizody/")

# Funguje i kratsi varianta.
csfd_fetch("/film/69432/epizody/")
```

Stažení profilu herce:

``` r
# Text za ciselnym identifikatorem ("-patrick-stewart") neni nutny, ale ani nevadi.
csfd_fetch("/tvurce/2052-patrick-stewart")
```

Stažení stránky s filmovými cenami. Důležitá je v tomto případě i část
adresy za otazníkem, která určuje rozmezí let, v nichž bylo ocenění
uděleno:

``` r
csfd_fetch("/oceneni/2-cesky-lev/?yearsSpan=2002-2011")
```

Stažení stránky s uživatelskými recenzemi:

``` r
csfd_fetch("/film/69432-star-trek-nova-generace/recenze/?page=2")
```

## Vyhledávání

K vyhledávání slouží tři různé funkce:

-   [Podrobné vyhledávání](https://www.csfd.cz/podrobne-vyhledavani/):
    `csfd_search_titles()`
-   [Tvůrci](https://www.csfd.cz/podrobne-vyhledavani/tvurci/):
    `csfd_search_creators()`
-   [Žebříčky](https://www.csfd.cz/zebricky/vlastni-vyber/):
    `csfd_search_ranks()`

Příklad: Seznam českých seriálů od roku 1989, řazený podle průměru
hodnocení. Narozdíl od `csfd_fetch()` není třeba URL stránky, ale
hodnoty vybraných parametrů (pro zobrazení nápovědy použijte příkaz
`?csfd_search`).

``` r
tv_shows <- csfd_search_titles(
  type = "tv_show",
  origin = list(any = c("Česko", "Československo")),
  released = c(1989, 2022),
  sort = "rating_average"
)

tv_shows$results
```

Vyhledání akčních filmů herečky Michelle Yeoh - číselný identifikátor
pochází z URL adresy její [stránky na
ČSFD](https://www.csfd.cz/tvurce/1818-michelle-yeoh/prehled/).

``` r
csfd_search_titles(
  type   = c("film", "tv_film"),
  genre  = "Akční",
  fields = list(actor = 1818),
  sort   = "year_asc"
)
```

## Vlastní scrapery

Balíček má pěkné pokrytí stránek filmů, seriálů a podobně. Stáhnout si
ale můžete libovolnou ČSFD stránku a data seškrabat po svém, a to
funkcemi z balíčků [rvest](https://rvest.tidyverse.org/) a
[xml2](https://xml2.r-lib.org/).

Zdrojový HTML kód lze získat přes pole `html`:

``` r
tng$html
#> {html_document}
#> <html lang="cs-CZ">
#> [1] <head>\n<meta http-equiv="Content-Type" content="text/html; charset=UTF-8 ...
#> [2] <body id="top">\n\n\t\t<div class="body">\n\t\t\t<div class="ad-wrapper"> ...
#> [3] <a href="#top" title="Zpět nahoru" id="back-to-top"><i class="icon icon-a ...
```

Takto získaný objekt typu `<xml_document>` nepřetrvá do příšího spuštění
Rka, a nemá smysl jej ukládat na disk. Jedná se pouze o tzv. external
pointer, který po ukončení Rka už na nic neukazuje. Je ale možné uložit
celý `<csfd_scraper>` objekt, který uchovává HTML v podobě syrových
bajtů, a v případě potřeby ho znovu načte do paměti (takže volání
`tng$html` v příkladu bude fungovat vždycky).

``` r
saveRDS(tng, "tng.rds")
```

Případně zapsat HTML kód jako text:

``` r
xml2::write_html(tng$html, "tng.html")
```

## Nahlašování změn ve struktuře stránek

Změny ve HTML struktuře stránek mohou mít za následek nesprávná nebo
chybějící data. Protože je tento nástroj neoficiální a není vyvinut v
součinnosti s ČSFD, není možné dopředu vědět, kdy se který HTML tag na
které stránce změní.

Pokud zjistíte, že vám `<csfds_scraper>` vrací nesprávná nebo chybějící
data, nahlašte to prosím
[zde](https://github.com/jchrom/csfd/issues/new).

## Licence

csfd podléhá licenčnímu ujednání [MIT](LICENSE.md).
