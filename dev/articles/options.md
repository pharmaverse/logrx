# logrx Options

One of the ways that `logrx` has been built to be customizable is to add
a set of options to enable different logging and execution elements.

Below are the options and their default values

| option                              | value                  | description                                           |
|:------------------------------------|:-----------------------|:------------------------------------------------------|
| log.rx                              | An empty R environment | Used to store log elements during program exection    |
| log.rx.exec.env                     | NULL                   | The environment in which the program code is executed |
| [log.rx.lint](#log.rx.lint)         | FALSE                  | A `lintr` object for use in lint checking             |
| [log.rx.approved](#log.rx.approved) | ./approved.rds         | Location of an approved functions file                |

## log.rx.lint

*“lintr provides static code analysis for R. It checks for adherence to
a given style, identifying syntax errors and possible semantic issues,
then reports them to you so you can take action.”*

  

Linting can help enforce best practice for a variety of topics including
code readability, efficiency, style, consistency, etc. You can find all
available linters
[here](https://lintr.r-lib.org/reference/linters.html), or [create your
own](https://lintr.r-lib.org/articles/creating_linters.html).

  

If you or your organization would like to implement any linters, you can
set your `log.rx.lint` option globally so your specific set of checks
are run and their results are recorded to your log for every script
executed.

  

It is recommended to use the
[`lintr::library_call_linter()`](https://lintr.r-lib.org/reference/library_call_linter.html).
This is to ensure `logrx` will find the correct package and functions
used.

  

Hester J, Angly F, Hyde R, Chirico M, Ren K, Rosenstock A, Patil I
(2022). lintr: A ‘Linter’ for R Code. <https://github.com/r-lib/lintr>,
<https://lintr.r-lib.org>.

## log.rx.approved

See [Logging Unapproved Package and Function
Use](https://pharmaverse.github.io/logrx/dev/articles/approved.md) for
additional details.
