#' Remove hard line breaks in clipboard then paste
#'
#' \code{unwrap} Remove unneeded hard line breaks of text in clipboard, like
#' copied from pdf, then paste into current cursor position.
#'
#' @param extra_blank_line Whether to insert extra blank line between paragraphs
#'
#' @return
#' @export
#'
unwrap <- function(extra_blank_line = FALSE) {
    clipboard <- stringr::str_trim(readClipboard(), side = "right")
    # use character class [] because each symbol are single characters, no need to use |. the Chinese quote have to be inside a double quote
    non_terminating_match<- stringr::str_c("[^\\.!?:", "”", "]") # terminating punctuation in end of line.
    # str_view_all(clipboard, str_c(".*", non_terminating_match, "$"))
    to_remove_wrap <- stringr::str_detect(clipboard, stringr::str_c(".*", non_terminating_match, "$"))
    # use space for soft wrap lines, new line for other wrap
    line_connector <- rep(ifelse(extra_blank_line, "\n\n", "\n"), length(clipboard))
    line_connector[to_remove_wrap] <- " "
    text_formated <- stringr::str_c(clipboard, line_connector, collapse = "")
    context <- rstudioapi::getActiveDocumentContext()
    rstudioapi::insertText(context$selection[[1]]$range, text_formated, id = context$id)
}
