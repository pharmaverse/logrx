# Changelog

## logrx (development version)

### New Features

### Updates

- Improved error messaging when a log.rx environment already exists
  ([\#276](https://github.com/pharmaverse/logrx/issues/276))
  - In non-interactive mode, provides clear guidance on how to resolve
    the issue
  - In interactive mode, offers users the option to automatically remove
    the existing environment or handle it manually
  - Added comprehensive tests for both interactive and non-interactive
    scenarios
- Removed .dcf file for old Addin
  ([\#280](https://github.com/pharmaverse/logrx/issues/280))

### Documentation

- Updated
  [`axecute()`](https://pharmaverse.github.io/logrx/dev/reference/axecute.md)
  documentation to correctly reflect return behavior
  ([\#288](https://github.com/pharmaverse/logrx/issues/288))

## logrx 0.4.0

CRAN release: 2025-05-05

### New Features

- Adds the optional `extra_info` parameter to
  [`axecute()`](https://pharmaverse.github.io/logrx/dev/reference/axecute.md)
  which lets users pass a list object through to the extra info section
  of the log. The object will be printed according to the YAML format
  ([\#180](https://github.com/pharmaverse/logrx/issues/180))

### Updates

- The Shiny Addin has been removed from this Package
  ([\#207](https://github.com/pharmaverse/logrx/issues/207))

  - A standalone package for the Shiny Addin has been created
  - See [logrxaddin](https://github.com/pharmaverse/logrxaddin) for more
    information

- Adds unit tests for `write_repo_urls()` and `get_repo_urls()` internal
  functions.

- Extend testing unexported function use
  ([\#208](https://github.com/pharmaverse/logrx/issues/208))

- Swap in
  [`lintr::library_call_linter()`](https://lintr.r-lib.org/reference/library_call_linter.html)
  for local version of `library_call_linter()`
  ([\#221](https://github.com/pharmaverse/logrx/issues/221))

- Update
  [`read_log_file()`](https://pharmaverse.github.io/logrx/dev/reference/read_log_file.md)
  for updated version of [sessioninfo](https://sessioninfo.r-lib.org)
  [\#246](https://github.com/pharmaverse/logrx/issues/246)

### Documentation

- Add vignette for creating a lockfile
  ([\#233](https://github.com/pharmaverse/logrx/issues/233))

## logrx 0.3.2

CRAN release: 2025-02-18

- Hotfix to update tests
  ([\#231](https://github.com/pharmaverse/logrx/issues/231))

## logrx 0.3.1

CRAN release: 2024-04-12

- Hotfix to update used and unapproved packages and functions writing
  ([\#218](https://github.com/pharmaverse/logrx/issues/218))

## logrx 0.3.0

CRAN release: 2023-10-17

- Moved website theme to bootstrap 5, enabled search
  ([\#179](https://github.com/pharmaverse/logrx/issues/179))

- Add `show_repo_url` option in
  [`axecute()`](https://pharmaverse.github.io/logrx/dev/reference/axecute.md)
  to capture repo URL(s) into log file
  ([\#167](https://github.com/pharmaverse/logrx/issues/167))

- Moved website theme to Bootstrap 5, enabled search
  ([\#179](https://github.com/pharmaverse/logrx/issues/179))

- Add `include_rds` argument to
  [`axecute()`](https://pharmaverse.github.io/logrx/dev/reference/axecute.md)
  to export log as rds file

- Add
  [`read_log_file()`](https://pharmaverse.github.io/logrx/dev/reference/read_log_file.md)
  to read logrx log file as optional function

- Add `library_call_linter()` to ensure all library calls are at the top
  of the script
  ([\#163](https://github.com/pharmaverse/logrx/issues/163))

- Remove argument for remove_log_object from
  [`axecute()`](https://pharmaverse.github.io/logrx/dev/reference/axecute.md)
  still accessible via
  [`log_write()`](https://pharmaverse.github.io/logrx/dev/reference/log_write.md)
  ([\#182](https://github.com/pharmaverse/logrx/issues/182))

- Added functionality so
  [`axecute()`](https://pharmaverse.github.io/logrx/dev/reference/axecute.md)
  works with `.Rmd` files
  ([\#140](https://github.com/pharmaverse/logrx/issues/140))

- R Version switched from `>3.5` to `>4.0` in `DESCRIPTION` file
  ([\#198](https://github.com/pharmaverse/logrx/issues/198))

## logrx 0.2.2

CRAN release: 2023-06-16

- Hotfix to remove unnecessary
  [`across()`](https://dplyr.tidyverse.org/reference/across.html) and
  update `.data$var` top new syntax to match updates in source packages
  ([\#172](https://github.com/pharmaverse/logrx/issues/172))
- Add [dplyr](https://dplyr.tidyverse.org) version requirement

## logrx 0.2.1

CRAN release: 2023-02-03

- non-function objects are no longer returned as functions by
  `get_used_functions`
  ([\#154](https://github.com/pharmaverse/logrx/issues/154))

## logrx 0.2.0

CRAN release: 2023-01-18

- Major update release for logrx

- Package has moved from `atorus-research` to `pharmaverse`
  organization.

### New Features

- [logrx](https://pharmaverse.github.io/logrx/) now produces a unique
  hash sum for each log file and script
  ([\#29](https://github.com/pharmaverse/logrx/issues/29))

### Updates

- Updated the run environment to be a child of the global environment
  instead of a child of the `logrx` package namespace. This fixes the
  issue of `logrx` using internally imported versions of functions in
  the place of user imported version of functions.
  ([\#104](https://github.com/pharmaverse/logrx/issues/104))

- `get_used_functions()` now works for scripts with no functions
  ([\#111](https://github.com/pharmaverse/logrx/issues/111))

### Documentation

- Add options vignette and move all options for `logrx` to be set on
  package load if not set by user prior
  ([\#110](https://github.com/pharmaverse/logrx/issues/110))([\#124](https://github.com/pharmaverse/logrx/issues/124))

- Added vignette showcasing
  [logrx](https://pharmaverse.github.io/logrx/) working with
  [tidylog](https://github.com/elbersb/tidylog/) package
  ([\#97](https://github.com/pharmaverse/logrx/issues/97))

- Update documentation to exported and non-exported functions
  ([\#106](https://github.com/pharmaverse/logrx/issues/106))([\#120](https://github.com/pharmaverse/logrx/issues/120))

- Fix typo in and re-format approved vignette
  ([\#105](https://github.com/pharmaverse/logrx/issues/105))

- Update logrx vignette to describe log attributes in more details
  ([\#119](https://github.com/pharmaverse/logrx/issues/119))([\#136](https://github.com/pharmaverse/logrx/issues/136))([\#137](https://github.com/pharmaverse/logrx/issues/137))

- Add vignette to execution vignette to help users better understand
  executing code and the different ways it can be done
  ([\#72](https://github.com/pharmaverse/logrx/issues/72))([\#73](https://github.com/pharmaverse/logrx/issues/73))

## logrx 0.1.1

CRAN release: 2022-09-12

Address CRAN comments about documentation.

## logrx 0.1.0

CRAN release: 2022-06-17

Beta release for logrx

### New Features

- Add `to_report` param to
  [`axecute()`](https://pharmaverse.github.io/logrx/dev/reference/axecute.md)
  to give users ability to filter out which pieces are reported
- Add return codes when running
  [`axecute()`](https://pharmaverse.github.io/logrx/dev/reference/axecute.md)
  to determine if there were errors
- Improve approved packages use with new function
  [`build_approved()`](https://pharmaverse.github.io/logrx/dev/reference/build_approved.md)
  and corresponding vignette

### Documentation

- Add example articles for adsl and a risk difference table

## logrx 0.0.1

Initial alpha release of logrx

See the [GitHub release
tracker](https://github.com/pharmaverse/logrx/releases) for additional
release documentation and links to issues.
