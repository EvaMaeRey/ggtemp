
  - [*To the reader*](#to-the-reader)
  - [Part 0. Proposal](#part-0-proposal)
  - [Part I. Work out functionality ✅](#part-i-work-out-functionality-)
      - [Try it out](#try-it-out)
  - [Part II. Packaging and documentation 🚧
    ✅](#part-ii-packaging-and-documentation--)
      - [Phase 1. Minimal working
        package](#phase-1-minimal-working-package)
          - [Bit A. Created files for package archetecture, running
            `devtools::create(".")` in interactive session. 🚧
            ✅](#bit-a-created-files-for-package-archetecture-running-devtoolscreate-in-interactive-session--)
          - [Bit B. Added roxygen skeleton? 🚧
            ✅](#bit-b-added-roxygen-skeleton--)
          - [Bit C. Managed dependencies ? 🚧
            ✅](#bit-c-managed-dependencies---)
          - [Bit D. Moved functions R folder? 🚧
            ✅](#bit-d-moved-functions-r-folder--)
          - [Bit E. Run `devtools::check()` and addressed errors. 🚧
            ✅](#bit-e-run-devtoolscheck-and-addressed-errors--)
          - [Bit F. Build package 🚧 ✅](#bit-f-build-package--)
          - [Bit G. Write and test traditional README that uses built
            package. 🚧
            ✅](#bit-g-write-and-test-traditional-readme-that-uses-built-package--)
          - [Bit H. Chosen a license? 🚧 ✅](#bit-h-chosen-a-license--)
          - [Bit I. Add lifecycle badge
            (experimental)](#bit-i-add-lifecycle-badge-experimental)
      - [Phase 2: Listen & iterate 🚧 ✅](#phase-2-listen--iterate--)
      - [Phase 3: Let things settle](#phase-3-let-things-settle)
          - [Bit A. Settle on examples. Put them in the roxygen skeleton
            and readme. 🚧
            ✅](#bit-a-settle-on-examples-put-them-in-the-roxygen-skeleton-and-readme--)
          - [Bit B. Written formal tests of functions and save to test
            that folders 🚧
            ✅](#bit-b-written-formal-tests-of-functions-and-save-to-test-that-folders--)
          - [Bit C. Added a description and author information in the
            DESCRIPTION file 🚧
            ✅](#bit-c-added-a-description-and-author-information-in-the-description-file--)
          - [Bit D. Addressed *all* notes, warnings and errors. 🚧
            ✅](#bit-d-addressed-all-notes-warnings-and-errors--)
      - [Phase 4. Promote to wider
        audience…](#phase-4-promote-to-wider-audience)
          - [Bit A. Package website built? 🚧
            ✅](#bit-a-package-website-built--)
          - [Bit B. Package website deployed? 🚧
            ✅](#bit-b-package-website-deployed--)
      - [Phase 5: Harden/commit](#phase-5-hardencommit)
          - [Submit to CRAN? 🚧 ✅](#submit-to-cran--)
  - [Appendix: Reports, Environment](#appendix-reports-environment)
      - [Description file extract](#description-file-extract)
      - [Environment](#environment)
      - [`devtools::check()` report](#devtoolscheck-report)
      - [Non-developer introduction to package (and test of installed
        package)](#non-developer-introduction-to-package-and-test-of-installed-package)
      - [Example using package](#example-using-package)

# *To the reader*

Welcome to the R package building helper *readme2pkg.template*\!

Below, is a readme that provides steps for building a package. This
readme acts as a worksheet, checklist, and control document as functions
used in package building are included within.

We’ll use the `{readme2pkg}` helper package to send code chunks to
different directories in the package.

To install `{readme2pkg}`:

``` 

remotes::install.github("EvaMaeRey/readme2pkg")
```

# Part 0. Proposal

Proposing the {xxxx} package\! 🦄
<!-- (typical package introduction write up; but actually aspirational) -->

The goal of {xxxx} is to make … easier.

Without the package, we live in the effort-ful world that follows 🏋:

``` r
x <- 4

2*x
```

With the {xxxx} package, we’ll live in a different world (🦄 🦄 🦄) where
the task is a snap 🫰:

Proposed API:

``` 

library(xxxxx)

xxxxx::times_two(x = 4)
```

# Part I. Work out functionality ✅

Here is a function that will do some work…

``` r
times_two <- function(x){
  
  x*2
  
}
```

## Try it out

``` r
times_two(4)
```

# Part II. Packaging and documentation 🚧 ✅

## Phase 1. Minimal working package

### Bit A. Created files for package archetecture, running `devtools::create(".")` in interactive session. 🚧 ✅

``` r
devtools::create(".")
```

### Bit B. Added roxygen skeleton? 🚧 ✅

Use a roxygen skeleton for auto documentation and making sure proposed
functions are *exported*. Generally, early on, I don’t do much
(anything) in terms of filling in the skeleton for documentation,
because things may change.

### Bit C. Managed dependencies ? 🚧 ✅

Package dependencies managed, i.e. `depend::function()` in proposed
functions and declared in the DESCRIPTION

``` r
usethis::use_package("ggplot2")
```

### Bit D. Moved functions R folder? 🚧 ✅

Use new {readme2pkg} function to do this from readme…

``` r
readme2pkg::chunk_to_r("times_two")
```

### Bit E. Run `devtools::check()` and addressed errors. 🚧 ✅

``` r
devtools::check(pkg = ".")
```

### Bit F. Build package 🚧 ✅

``` r
devtools::build()
```

### Bit G. Write and test traditional README that uses built package. 🚧 ✅

The goal of the {xxxx} package is to …

Install package with:

    remotes::installgithub("EvaMaeRey/readme2pkg.template")

Then…

``` r
library(readme2pkg.template)  ##<< change to your package name here
times_two(10)
```

### Bit H. Chosen a license? 🚧 ✅

``` r
usethis::use_mit_license()
```

### Bit I. Add lifecycle badge (experimental)

``` r
usethis::use_lifecycle_badge("experimental")
```

## Phase 2: Listen & iterate 🚧 ✅

Try to get feedback from experts on API, implementation, default
decisions. Is there already work that solves this problem?

## Phase 3: Let things settle

### Bit A. Settle on examples. Put them in the roxygen skeleton and readme. 🚧 ✅

### Bit B. Written formal tests of functions and save to test that folders 🚧 ✅

That would look like this…

``` r
library(testthat)

test_that("calc times 2 works", {
  expect_equal(times_two(4), 8)
  expect_equal(times_two(5), 10)
  
})
```

``` r
readme2pkg::chunk_to_tests_testthat("test_calc_times_two_works")
```

### Bit C. Added a description and author information in the DESCRIPTION file 🚧 ✅

### Bit D. Addressed *all* notes, warnings and errors. 🚧 ✅

## Phase 4. Promote to wider audience…

### Bit A. Package website built? 🚧 ✅

### Bit B. Package website deployed? 🚧 ✅

## Phase 5: Harden/commit

### Submit to CRAN? 🚧 ✅

# Appendix: Reports, Environment

## Description file extract

``` r
# readlines(Description)
```

## Environment

Here I just want to print the packages and the versions

``` r
all <- sessionInfo() |> print() |> capture.output()
all[11:17]
```

## `devtools::check()` report

``` r
devtools::check(pkg = ".")
```

## Non-developer introduction to package (and test of installed package)

The goal of the {xxx} package

To install the dev version use the following:

    remotes::install_github("owner/repo") # 

## Example using package

``` r
library(mypackage)
myfunction(mtcars)
```
