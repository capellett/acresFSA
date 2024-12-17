
<!-- README.md is generated from README.Rmd. Please edit that file -->

# acresFSA

<!-- badges: start -->

<!-- badges: end -->

The acresFSA R-package contains a selection of data downloaded from the
United States Department of Agriculture (USDA) Farm Service Agency (FSA)
website.

> “Farm Service Agency policy requires that producers participating in
> several programs submit an annual report regarding all cropland use on
> their farms. These programs include Agriculture Risk Coverage (ARC)
> and Price Loss Coverage (PLC). Reporting also applies to those who
> receive marketing assistance loans or loan deficiency payments.
> Failure to file an accurate and timely acreage report for all crops
> and land uses can result in loss of program benefits. Producers are
> required to self report all cropland on each farm to FSA annually. FSA
> uses these data to determine payment eligibility (land must be in an
> eligible agricultural use to qualify for payments) and to calculate
> losses for various disaster programs. Data are reported in the
> following categories: planted; prevented planted; and failed. In
> addition, the National Agricultural Statistics Service uses FSA
> planted acreage data to complement their survey data.”

<https://www.fsa.usda.gov/news-room/efoia/electronic-reading-room/frequently-requested-information/crop-acreage-data/index>

# Getting Started

The “Getting Started” article is intended to be accessible to the
general public. It includes a download link and an interactive data
exploration tool.

# For R Users

The acresFSA R package exports a single data object, named `acresFSA`.
The package also contains articles (AKA vignettes), plotting functions,
and a few custom helper functions. The vignettes document the methods
used to compile and clean up the data.

You can install the development version of acresFSA from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("capellett/acresFSA")
```
