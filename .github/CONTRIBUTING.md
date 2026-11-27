# Contributing to logrx

This outlines how to propose a change to logrx.
For a detailed discussion on contributing to this and other tidyverse packages, please see the [development contributing guide](https://rstd.io/tidy-contrib) and our [code review principles](https://code-review.tidyverse.org/).

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 
If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).
See our guide on [how to create a great issue](https://code-review.tidyverse.org/issues/) for more advice.

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("pharmaverse/logrx", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

### Version bumps

After a push or PR merge to `main`, the version-bump workflow commits the next
development version directly to `DESCRIPTION`. Opening or updating a PR does
not change the version on `main`. The workflow can also be run manually.

For a package release, include `[skip vbump]` in the commit message pushed to
`main` (for a merge or squash merge, include it in the final merge commit
message). This preserves the release version. After releasing, run the workflow
manually on `main` with **Start the next development version after a release**
selected. This starts the next development version and retains the release
notes under their own heading.

The workflow uses `GITHUB_TOKEN` with contents write access by default. If it
cannot push to `main` due to branch protection, an administrator must configure
an appropriate bot token as the `REPO_GITHUB_TOKEN` repository secret, grant
that bot bypass permission, and set the repository variable
`VBUMP_USE_REPO_GITHUB_TOKEN` to `true`. Callers of the reusable workflow can
instead pass the secret and set the `use-repo-github-token` input to `true`.
Pushes made with `GITHUB_TOKEN` do not trigger other push workflows; use the
bot token option if those workflows must run on the bump commit.
The workflow's own bump commits are ignored to prevent a loop.

### Code style

*   New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
    You can use [Air](https://posit-dev.github.io/air/) to apply this style, but please don't restyle code that has nothing to do with your PR.  

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

## Code of Conduct

Please note that the logrx project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
