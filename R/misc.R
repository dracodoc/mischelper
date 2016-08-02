#' Unwrap text
#'
#' \code{unwrap} Remove unneeded hard line breaks of text in clipboard, then
#' paste into current cursor position.
#'
#' @param extra_blank_line Whether to insert extra blank line between paragraphs.
#'
#' @export
#'
unwrap <- function(extra_blank_line = FALSE) {
    clipboard <- stringr::str_trim(utils::readClipboard(), side = "right")
    # use character class [] because each symbol are single characters, no need to use |. the Chinese quote have to be inside a double quote
    non_terminating_match <- stringr::str_c("[^\\.!?:", "\\u201d", "]") # terminating punctuation in end of line.
    # str_view_all(clipboard, str_c(".*", non_terminating_match, "$"))
    to_remove_wrap <- stringr::str_detect(clipboard, stringr::str_c(".*", non_terminating_match, "$"))
    # use space for soft wrap lines, new line for other wrap
    line_connector <- rep(ifelse(extra_blank_line, "\n\n", "\n"), length(clipboard))
    line_connector[to_remove_wrap] <- " "
    text_formated <- stringr::str_c(clipboard, line_connector, collapse = "")
    context <- rstudioapi::getActiveDocumentContext()
    rstudioapi::insertText(context$selection[[1]]$range, text_formated, id = context$id)
}

#' Unwrap with blank line
#'
#' Remove unneeded hard line breaks of text in clipboard, insert extra blank
#' line between paragraphs, then paste into current cursor position.
#'
#' Need this helper because RStudio Addin doesn't support function with
#' parameters.
#'
#' @export
#'
unwrap_extra_blank <- function(){
    unwrap(TRUE)
}

#' Flip windows path
#'
#' \code{flip_windows_path} convert "\" in clipboard to "/", then paste into
#' current cursor position
#'
#' @export
#'
flip_windows_path <- function(){
    p2 <- stringr::str_replace_all(utils::readClipboard(), "\\\\", "/")
    context <- rstudioapi::getActiveDocumentContext()
    rstudioapi::insertText(context$selection[[1]]$range, p2, id = context$id)
}

#' microbenchmark selected
#'
#' microbenchmark selected code for 10 runs in console without changing the
#' source code.
#'
#' \code{microbenchmark()} parameters can be changed by recalling history in
#' console then editing the last line.
#'
#' @export
#'
benchmark <- function(){
    context <- rstudioapi::getActiveDocumentContext()
    selection_start <- context$selection[[1]]$range$start
    selection_end <- context$selection[[1]]$range$end
    if (any(selection_start != selection_end)) { # text selected
        selected <- context$selection[[1]]$text
        formated <- stringr::str_c("microbenchmark::microbenchmark({\n",
                                   selected, "}, times = 10)")
        rstudioapi::sendToConsole(formated, execute = TRUE)
    }
}

#' profvis selected
#'
#' profvis selected code in console without changing source code.
#'
#' @export
#'
profv <- function(){
    # if (!requireNamespace("profvis", quietly = TRUE)) {
    #     stop("profvis needed but not automatically installed.\nInstall the package with install.packages(\"profvis\")",
    #          call. = FALSE)
    # }
    context <- rstudioapi::getActiveDocumentContext()
    selection_start <- context$selection[[1]]$range$start
    selection_end <- context$selection[[1]]$range$end
    if (any(selection_start != selection_end)) { # text selected
        selected <- context$selection[[1]]$text
        formated <- stringr::str_c("profvis::profvis({\n",
                                   selected, "})")
        rstudioapi::sendToConsole(formated, execute = TRUE)
    }
}