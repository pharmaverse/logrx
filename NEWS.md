
# logrx 0.3.0

- Add `show_repo_url` option in `axecute()` to capture repo URL(s) into log file (#167)
- Moved website theme to bootstarp 5, enabled search (#179)
- Add `include_rds` argument to `axecute()` to export log as rds file
- Add `read_log_file()` to read previous logrx log file


# logrx 0.2.2

 - Hotfix to remove unnecessary `across()` and update `.data$var` top new syntax to match updates in source packages (#172)
 - Add `{dplyr}` version requirement

# logrx 0.2.1

 - non-function objects are no longer returned as functions by `get_used_functions` (#154)

# logrx 0.2.0

 - Major update release for logrx
 
 - Package has moved from `atorus-research` to `pharmaverse` organization.


## New Features 

  - `{logrx}` now produces a unique hash sum for each log file and script (#29)

## Updates

  - Updated the run environment to be a child of the global environment instead of a child of the `logrx` package namespace.  This fixes the issue of `logrx` using internally imported versions of functions in the place of user imported version of functions. (#104)
  
  - `get_used_functions()` now works for scripts with no functions (#111)

## Documentation

  - Add options vignette and move all options for `logrx` to be set on package load if not set by user prior (#110)(#124)

  - Added vignette showcasing `{logrx}` working with `{tidylog}` package (#97)
  
  - Update documentation to exported and non-exported functions (#106)(#120)

  - Fix typo in and re-format approved vignette (#105)

  - Update logrx vignette to describe log attributes in more details (#119)(#136)(#137)

  - Add vignette to execution vignette to help users better understand executing code and the different ways it can be done (#72)(#73)

# logrx 0.1.1

Address CRAN comments about documentation.

# logrx 0.1.0

Beta release for logrx 

## New Features 

  - Add `to_report` param to `axecute()` to give users ability to filter out which pieces are reported
  - Add return codes when running `axecute()` to determine if there were errors
  - Improve approved packages use with new function `build_approved()` and corresponding vignette

## Documentation

  - Add example articles for adsl and a risk difference table

# logrx 0.0.1

Initial alpha release of logrx

See the [GitHub release tracker](https://github.com/pharmaverse/logrx/releases) for additional release documentation and links to issues. 
