
#' Get functions used within a file
#'
#' @param file File path of file to run
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' file <- "ex_file_to_lint.R"
#' get_functions_used(file)
#' }
get_used_functions <- function(file,
                               function_tokens = c("SYMBOL_FUNCTION_CALL", "SPECIAL"),
                               add_library = TRUE){

   parsed_content <- parse(file = file, keep.source = TRUE)

   tokens <- utils::getParseData(parsed_content)

   if (any(tokens$token == "SYMBOL_PACKAGE")) {
      colon_call <- tokens$text[tokens$token == "SYMBOL_PACKAGE"]
      warning(colon_call, " called with `::` so it will not show in search path", call. = FALSE)
   }

   tokens <- tokens[tokens[["token"]] %in% function_tokens,]

   if(add_library) {
      with_library <- add_library(tokens)
      combined <- aggregate(text~library, with_library, unique)
      names(combined) <- c("library", "functions")
      combined
   }
}


#' Add libraries to functions
#'
#' Each script should be independent so we can use the search path since this
#' would be just for this script.
#' This must also be run after script execution.
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
add_library <- function(df){
   search_lookup <- purrr::map(search(), objects)
   names(search_lookup) <- search()
   df$library <- unlist(purrr::map(df$text, ~find_first(., search_lookup)))
   df
}


find_first <- function(func, search_lookup){
   flag_found <- purrr::map(search_lookup, ~ func %in% .)
   if (any(unlist(flag_found))) {
      names(flag_found[flag_found == TRUE][1])
   } else {
      NA
   }
}
