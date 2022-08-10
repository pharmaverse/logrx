# logrx 0.2.0

Updates 
  - Update the run environment to be a child of the global environment instead of a child of the `logrx` package namespace.  This fixes the issue of `logrx` using internally imported versions of functions in the place of user imported version of functions. (PR [#109](https://github.com/atorus-research/logrx/pull/109))

# logrx 0.1.0

Beta release for logrx with introduction of numerous new features:

Enhancements

  - Add `to_report` param to `axecute()` to give users ability to filter out which pieces are reported
  - Add return codes when running `axecute()` to determine if there were errors
  - Improve approved packages use with new function `build_approved()` and corresponding vignette
  - Add example articles for adsl and a risk difference table

# logrx 0.0.1

Initial alpha release of logrx

See the [GitHub release tracker](https://github.com/atorus-research/logrx/releases) for additional release documentation and links to issues. 
