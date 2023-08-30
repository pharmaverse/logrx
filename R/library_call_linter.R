#' Library call linter
#'
#' Force library calls to all be at the top of the script.
#'
#' @examples
#' library(lintr)
#'
#' # will produce lints
#' lint(
#'   text = "
#'     library(dplyr)
#'     print('test')
#'     library(tidyr)
#'   ",
#'   linters = library_call_linter()
#' )
#'
#' lint(
#'   text = "
#'     library(dplyr)
#'     print('test')
#'     library(tidyr)
#'     library(purrr)
#'   ",
#'   linters = library_call_linter()
#' )
#'
#' # okay
#' lint(
#'   text = "
#'     library(dplyr)
#'     print('test')
#'   ",
#'   linters = library_call_linter()
#' )
#'
#' lint(
#'   text = "
#'     # comment
#'     library(dplyr)
#'   ",
#'   linters = library_call_linter()
#' )
#'
#' @noRd
library_call_linter <- function() {

   if (!requireNamespace("lintr", quietly = TRUE)) {
      warning(strwrap("Library calls will not be checked to confirm all are at
         the top of the script. Install the lintr package to use this feature.",
         prefix = " ", initial = ""))
      return(list())
   }
   if (!requireNamespace("xml2", quietly = TRUE)) {
      warning(strwrap("Library calls will not be checked to confirm all are at
         the top of the script. Install the xml2 package to use this feature.",
         prefix = " ", initial = ""))
      return(list())
   }

  xpath <- "
    (//SYMBOL_FUNCTION_CALL[text() = 'library'])[last()]
      /preceding::expr
      /SYMBOL_FUNCTION_CALL[text() != 'library'][last()]
      /following::expr[SYMBOL_FUNCTION_CALL[text() = 'library']]
  "

  lintr::Linter(function(source_expression) {
    if (!lintr::is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    bad_expr <- xml2::xml_find_all(xml, xpath)

    if (length(bad_expr) == 0L) {
      return(list())
    }

    lintr::xml_nodes_to_lints(
      bad_expr,
      source_expression = source_expression,
      lint_message = "Move all library calls to the top of the script.",
      type = "warning"
    )
  })
}
