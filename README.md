-   [Functional niches of cleanerfish species are mediated by habitat
    use, cleaning intensity, and client
    selectivity](#functional-niches-of-cleanerfish-species-are-mediated-by-habitat-use-cleaning-intensity-and-client-selectivity)
    -   [Instructions](#instructions)
    -   [Details](#details)
    -   [Datasets](#datasets)
    -   [Supplemental info](#supplemental-info)

Functional niches of cleanerfish species are mediated by habitat use, cleaning intensity, and client selectivity
================================================================================================================

This repository contains the code and data to reproduce all tables and
figures presented in Côté & Brandl “Functional niches of cleanerfish
species are mediated by habitat use, cleaning intensity, and client
selectivity.” The abstract is pasted below:

1.  An animal’s functional niche is a complex, multidimensional
    construct, mediated by an individual’s morphology, physiology, and
    behaviour. Behavioural aspects of the niche can be difficult to
    quantify, as their expression is often subtle and tailored to an
    infinite number of different situations that involve sophisticated
    mechanisms such as mutualisms, species dominance, or fear effects.
2.  The extreme diversity of tropical fish assemblages has led to
    extensive debate over the extent to which species differ in their
    resource use and functional role. Ectoparasite removal by
    cleanerfish species is considered a behaviourally complex
    interspecific interactions in vertebrates, but differences in the
    services rendered by various species of cleanerfish, and potential
    consequences for the range of clients (i.e., resources) they
    attract, have rarely been examined.
3.  Here, we quantify differences among three coexisting species of
    morphologically similar cleaner wrasses (Labroides bicolor, L.
    dimidiatus, and L. pectoralis) in the global centre of marine
    biodiversity, the Coral Triangle.
4.  We found no clear taxonomic partitioning of clients among
    cleanerfishes. However, the three cleanerfish species exhibited
    distinct habitat preferences, and differed in their cleaning
    intensity: L. bicolor serviced the fewest species and clients,
    while L. pectoralis serviced the most clients and spent the most
    time cleaning.
5.  Accordingly, L. pectoralis showed no preference for clients based on
    client size or abundance, while both L. bicolor and L. dimidiatus
    had a higher likelihood of interacting with clients based on their
    size (larger client species in L. bicolor, smaller client species
    in L. dimidiatus) and abundance (more abundant client species for
    both).
6.  Our results suggest that the services rendered by the three species
    of cleanerfishes differ in their spatial availability, quality, and
    selectivity, thus permitting the coexistence of these species
    despite their ecological similarity. This, in turn, creates a
    complex seascape of species-specific cleaning services that
    underpins crucial biotic interactions in the ocean’s most diverse
    ecosystem.

Instructions
------------

All analyses for the current project were done in R, using the drake
pipeline [drake](https://github.com/ropensci/drake). You can use drake
to compile all models, figures, and tables. To do so, first install
`drake` from CRAN:

``` r
install.packages("drake")
```

Next you need to open an R session with working directory set to the
root of the project.

We use a number of packages, listed in `r/packages.R`. If needed,
missing packages should first be installed.

Then, to generate all figures, analyses, and tables, simply run:

``` r
drake::r_make()
```

All output will be automatically rendered inside the folder called
output.

Details
-------

The elements of this project include:

1.  data: this folder includes all the raw data necessary to run the
    script.
2.  output: this folder includes all outputs from the R-script.
3.  r: this folder contains three .R files, `packages.R`, `functions.R`,
    and `plan.R`.  
    `packages.R` contains all packages needed, `functions.R` contains
    all functions to reproduce the analyses, and `plan.R` provides the
    code that binds each step of the workflow.

Datasets
--------

1.  depth (labroides\_depth\_sjb.csv): Contains depth distributions of
    three cleanerfish species on two reefs in Indonesia. Each row is an
    observation, columns contain data on site (Pinnacle or Hoga Home
    Reef), depth, and species.

2.  habitat (labroides\_habitat\_sjb.csv): Contains information about
    habitat properties of territories and home ranges of the three
    Labroides species. Rows are specific cleaner wrasse individuals.
    Columns 1-4 contain the station (cleaner wrasse individuals),
    species (1 = L. dimidiatus, 2 = L. pectoralis, 3 = L. bicolor), life
    stage (age = juvenile or adult), and the number of fish present at a
    given station (no\_fish; only one individual observed per station).
    Columns 5-9 provide detailed benthic compositional data (relative
    cover), while columns 10 and 11 provide summarized proportions of
    live vs. dead habitat. Column 12 provides territory size in square
    meters and column 13 the depth of the specific territory.

3.  temp (labroides\_temporal\_sjb.csv): Contains data on temporal
    patterns in cleaning interactions. Rows are cleaning stations
    (replicates). Columns 1-3 provide metadata as previously, while the
    remaining columns provide the three interaction variables across the
    three time periods. spp = Number of client species; client = Number
    of clients; time = Seconds spent inspecting/cleaning; am =
    7h00-9h00; noon = 11h00-13h00; pm = 15h00-17h00.

4.  Cleaning interaction datasets:

    1.  clean.interac (labroides\_cleaninginteractions\_sjb.csv):  
        full dataset of interactions between client species (common name
        in column 1) and cleaner fishes (columns). Column names are
        station names, which can be cross-matched with species in the
        “stations” dataset.
    2.  clean.clients (labroides\_clients\_meta\_sjb.csv): metadata
        containing scientific and common names of all clients, as well
        as their families.
    3.  clean.stations (.coi = COI primer
        data)labroides\_sites\_meta\_sjb.csv): metadata containing
        species affiliation for each station ID. 1 = L. dimidiatus, 2
        = L. pectoralis, 3 = L. bicolor.
    4.  fishID.all (fishID.merged.all.csv): metadata containing common
        and scientific names of all mobile fish species present at Hoga
        Home reef.
    5.  fish.abun (labroides\_fishcounts\_sjb.csv): abundances of all
        fish species in the area, as determined by point counts. First
        column gives species names, rest of the columns are one point
        count each.
    6.  client.size (labroides\_client\_sizes.csv): maximum size of all
        client species as obtained from FishBase.

Supplemental info
-----------------

This paper was produced using the following software and associated
packages:

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value                       
    ##  version  R version 4.0.3 (2020-10-10)
    ##  os       macOS Big Sur 10.16         
    ##  system   x86_64, darwin17.0          
    ##  ui       X11                         
    ##  language (EN)                        
    ##  collate  en_US.UTF-8                 
    ##  ctype    en_US.UTF-8                 
    ##  tz       Europe/Berlin               
    ##  date     2021-04-26                  
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package     * version date       lib source        
    ##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.2)
    ##  callr         3.5.1   2020-10-13 [1] CRAN (R 4.0.2)
    ##  cli           2.2.0   2020-11-20 [1] CRAN (R 4.0.3)
    ##  crayon        1.3.4   2017-09-16 [1] CRAN (R 4.0.2)
    ##  desc          1.2.0   2018-05-01 [1] CRAN (R 4.0.2)
    ##  devtools      2.3.2   2020-09-18 [1] CRAN (R 4.0.2)
    ##  digest        0.6.27  2020-10-24 [1] CRAN (R 4.0.2)
    ##  ellipsis      0.3.1   2020-05-15 [1] CRAN (R 4.0.2)
    ##  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.1)
    ##  fansi         0.4.1   2020-01-08 [1] CRAN (R 4.0.2)
    ##  fs            1.5.0   2020-07-31 [1] CRAN (R 4.0.2)
    ##  glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.2)
    ##  htmltools     0.5.0   2020-06-16 [1] CRAN (R 4.0.2)
    ##  knitr         1.30    2020-09-22 [1] CRAN (R 4.0.2)
    ##  magrittr      2.0.1   2020-11-17 [1] CRAN (R 4.0.2)
    ##  memoise       1.1.0   2017-04-21 [1] CRAN (R 4.0.2)
    ##  pkgbuild      1.1.0   2020-07-13 [1] CRAN (R 4.0.2)
    ##  pkgload       1.1.0   2020-05-29 [1] CRAN (R 4.0.2)
    ##  prettyunits   1.1.1   2020-01-24 [1] CRAN (R 4.0.2)
    ##  processx      3.4.4   2020-09-03 [1] CRAN (R 4.0.2)
    ##  ps            1.4.0   2020-10-07 [1] CRAN (R 4.0.2)
    ##  R6            2.5.0   2020-10-28 [1] CRAN (R 4.0.2)
    ##  remotes       2.2.0   2020-07-21 [1] CRAN (R 4.0.2)
    ##  rlang         0.4.10  2020-12-30 [1] CRAN (R 4.0.2)
    ##  rmarkdown     2.5     2020-10-21 [1] CRAN (R 4.0.3)
    ##  rprojroot     2.0.2   2020-11-15 [1] CRAN (R 4.0.2)
    ##  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.2)
    ##  stringi       1.5.3   2020-09-09 [1] CRAN (R 4.0.2)
    ##  stringr       1.4.0   2019-02-10 [1] CRAN (R 4.0.2)
    ##  testthat      3.0.0   2020-10-31 [1] CRAN (R 4.0.2)
    ##  usethis       1.6.3   2020-09-17 [1] CRAN (R 4.0.2)
    ##  withr         2.3.0   2020-09-22 [1] CRAN (R 4.0.2)
    ##  xfun          0.19    2020-10-30 [1] CRAN (R 4.0.2)
    ##  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.0.2)
    ## 
    ## [1] /Library/Frameworks/R.framework/Versions/4.0/Resources/library

All code written by Simon J. Brandl
(<a href="mailto:simonjbrandl@gmail.com" class="email">simonjbrandl@gmail.com</a>
and
<a href="https://github.com/simonjbrandl" class="uri">https://github.com/simonjbrandl</a>).
Please contact me for any issue or question.
