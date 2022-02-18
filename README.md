
<!-- README.md is generated from README.Rmd. Please edit that file -->

# csfd

<!-- badges: start -->

[![R-CMD-check](https://github.com/jchrom/csfd/workflows/R-CMD-check/badge.svg)](https://github.com/jchrom/csfd/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Smyslem balíčku csfd je umožnit novinářům a akademickým pracovníkům
skrejpování veřejně přístupných dat z [Česko-Slovenské filmové databáze
(ČSFD)](https://www.csfd.cz). Balíček není oficiálním produktem ČSFD,
není určen ke komerčnímu využití a nepomůže se stahováním většího počtu
stránek.

Pokud někdy v budoucnu ČSFD uvede do provozu standardní webovou API (po
vzoru IMDB), bude balíček deprekován.

## Instalace

``` r
# install.packages("devtools")
devtools::install_github("jchrom/csfd")
```

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
#> date: 2022-02-18 19:05:10
#> body: 85.1 Kb
#>  $reviews,  $releases,  $summary,  $plots,  $jobs,  $titles,  $episodes,
#>  $ratings, and  $ranks
```

Vybraný scraper zavoláme stejně, jako když vracíme hodnotu při běžném
indexování, například použitím `$`:

``` r
tng$summary
#> # A tibble: 1 × 9
#>   title      id      genre    origin released ended time_min time_max time_total
#>   <chr>      <chr>   <chr>    <chr>     <int> <int>    <int>    <int>      <int>
#> 1 Star Trek… film/4… Sci-Fi … USA        1989  1990       45       60       1219
```

To je v zásadě vše. `<csfds_scraper>` pozná, o jaký typ stránky se
jedná, takže máte vždy k dispozici ty správné scrapery.

Občas se stane, že má vrácená tabulka nula řádků, pokud určitá data na
stránce chybí. Například tag cloud bývá na stránkách filmů/seriálů, ale
pokud nikdo dané dílo neotagoval, data budou logicky chybět.

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

## Vlastní scrapery

Balíček má pěkné pokrytí stránek filmů, seriálů a podobně. Stáhnout si
ale můžete libovolnou ČSFD stránku a data seškrabat po svém, např.
funkcemi z balíčků [rvest](https://rvest.tidyverse.org/) a
[xml2](https://xml2.r-lib.org/).

Zdrojový HTML kód lze získat přes pole `html`:

``` r
tng$html
#> {html_document}
#> <html lang="cs-CZ">
#> [1] <head>\n<meta http-equiv="Content-Type" content="text/html; charset=UTF-8 ...
#> [2] <body id="top">\n\n\n\t\t<div class="ad-wrapper">\n\t\t\t<div id="leaderb ...
#> [3] <a href="#top" title="Zpět nahoru" id="back-to-top"><i class="icon icon-a ...
```

Takto získaný objekt typu `<xml_document>` nepřetrvá do příšího spuštění
Rka, a nemá smysl jej ukládat na disk. Jedná se pouze o externí pointer,
který po ukončení Rka už na nic neukazuje.

Je ale možné uložit celý `<csfd_scraper>` objekt, který uchovává HTML v
podobě syrových bajtů, a v případě potřeby ho znovu načte do paměti
(takže volání `tng$html` v příkladu bude fungovat vždycky).

## Licence

csfd podléhá licenčnímu ujednání [MIT](LICENSE.md).
