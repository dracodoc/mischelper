
#' read clipboard into data frame
#'
#' Read windows/mac/linux clipboard, convert csv into data frame, open data
#' viewer, write markdown table back to clipboard, return the data frame
#'
#' @return the data frame
#' @export
#'
clip_2_df <- function(){
  lines <- clipr::read_clip()
  # paste0(lines, collapse = "\n")
  # getting regular data frame so we can use the matrix indexing in next line
  df <- data.table::fread(paste0(lines, collapse = "\n"), na.strings = NULL, data.table = FALSE)
  # we don't want to print NA in table since the actual data don't have NA returned
  df[is.na(df)] <- ""
  View(df)
  clipr::write_clip(knitr::kable(df, format = "markdown"))
  return(df)
}

#' Read windows/mac clipboard into lines
#'
#' If windows, call \code{utils::readClipboard()}. If mac os, use
#' \code{pipe("pbpaste")}.
#'
#' @return character vector
#' @export
#' @examples
#' clip_read_lines()
clip_read_lines <- function(){
  os <- Sys.info()[['sysname']]
  if (os == "Windows") {
    return(utils::readClipboard())
  } else if (os == "Darwin") {
    pb_read_lines <- function(){
      clip_r_mac <- pipe("pbpaste")
      lines <- readLines(clip_r_mac)
      close(clip_r_mac)
      return(lines)
    }
    return(pb_read_lines())
  }
}

#' Write lines into windows/mac clipboard
#'
#' If windows, call \code{utils::writeClipboard()}. If mac os, use
#' \code{pipe("pbcopy", "w")}.
#'
#' Note there could be an extra new line in the end for mac os version.
#'
#' @param lines character vector
#'
#' @export
#' @examples
#' clip_write_lines(c("line 1", "line 2 \n and line 3"))
clip_write_lines <- function(lines) {
  os <- Sys.info()[['sysname']]
  if (os == "Windows") {
    return(utils::writeClipboard(lines))
  } else if (os == "Darwin") {
    pb_write_lines <- function(lines){
      clip_w_mac <- pipe("pbcopy", "w")
      # if using write, will have extra new line in end
      cat(lines, file = clip_w_mac, sep = "\n")
      close(clip_w_mac)  # close to flush
    }
    return(pb_write_lines(lines))
  }
}

#' Unwrap text
#'
#' \code{unwrap} Remove unneeded hard line breaks of text in clipboard, then
#' update the clipboard.
#'
#' @param extra_blank_line Whether to insert extra blank line between paragraphs.
#'
#' @export
#'
unwrap <- function(extra_blank_line = FALSE) {
  # clipboard <- stringr::str_trim(utils::readClipboard(), side = "right")
  clipboard <- stringr::str_trim(clip_read_lines(), side = "right")
  # use character class [] because each symbol are single characters, no need to use |. the Chinese quote have to be inside a double quote
  non_terminating_match <- stringr::str_c("[^\\.!?:", "\\u201d", "]") # terminating punctuation in end of line.
  # str_view_all(clipboard, str_c(".*", non_terminating_match, "$"))
  to_remove_wrap <- stringr::str_detect(clipboard, stringr::str_c(".*", non_terminating_match, "$"))
  # use space for soft wrap lines, new line for other wrap
  line_connector <- rep(ifelse(extra_blank_line, "\n\n", "\n"), length(clipboard))
  line_connector[to_remove_wrap] <- " "
  text_formated <- stringr::str_c(clipboard, line_connector, collapse = "")
  clip_write_lines(text_formated)
  # context <- rstudioapi::getActiveDocumentContext()
  # rstudioapi::insertText(context$selection[[1]]$range, text_formated, id = context$id)
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
  p2 <- stringr::str_replace_all(clip_read_lines(), "\\\\", "/")
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
benchmark <- function(runs = 10){
  context <- rstudioapi::getActiveDocumentContext()
  selection_start <- context$selection[[1]]$range$start
  selection_end <- context$selection[[1]]$range$end
  if (any(selection_start != selection_end)) { # text selected
    selected <- context$selection[[1]]$text
    formated <- stringr::str_c("microbenchmark::microbenchmark(selected_code = {\n",
      selected, "}, times = ", runs, ")")
    rstudioapi::sendToConsole(formated, execute = TRUE)
  }
}

#' time selected code
#'
#' Easier way to measure time cost of expression or selected code. Underthehood
#' it's just microbenchmark run once instead of 10.
#'
#' @export
#'
time_it <- function(){
  benchmark(1)
}

#' Render RMarkdown in global environment
#'
#' Knit document will start from scratch, this will use global environment
#' instead. So you don't have to run some expensive operations like read huge
#' file from disk every time in rendering.
#'
#' @export
#'
render_rmd <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  formated <- paste0('rmarkdown::render("', context$path, '")')
  rstudioapi::sendToConsole(formated, execute = TRUE)
}

# not used now since there is menu in RStudio
# profv <- function(){
#   # if (!requireNamespace("profvis", quietly = TRUE)) {
#   #     stop("profvis needed but not automatically installed.\nInstall the package with install.packages(\"profvis\")",
#   #          call. = FALSE)
#   # }
#   context <- rstudioapi::getActiveDocumentContext()
#   selection_start <- context$selection[[1]]$range$start
#   selection_end <- context$selection[[1]]$range$end
#   if (any(selection_start != selection_end)) { # text selected
#     selected <- context$selection[[1]]$text
#     formated <- stringr::str_c("profvis::profvis({\n",
#       selected, "})")
#     rstudioapi::sendToConsole(formated, execute = TRUE)
#   }
# }

#' View selected list with listviewer
#'
#' Select a list, view it with listviewer in viewer pane
#'
#' @export
view_list <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  selection_start <- context$selection[[1]]$range$start
  selection_end <- context$selection[[1]]$range$end
  if (any(selection_start != selection_end)) { # text selected
    selected <- context$selection[[1]]$text
    formated <- stringr::str_c("listviewer::jsonedit(",
      selected, ', mode = "view", modes = c("code", "view"))')
    rstudioapi::sendToConsole(formated, execute = TRUE)
  }
}

#' View selected data frame
#'
#' The RStudio Environment pane variable name column is too narrow for long
#' names, so difficutl to find the right data frame in long list of similar
#' names. Select the variable and use shortcut to use data viewer on it.
#'
#' @export

view_df <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  selection_start <- context$selection[[1]]$range$start
  selection_end <- context$selection[[1]]$range$end
  if (any(selection_start != selection_end)) { # text selected
    selected <- context$selection[[1]]$text
    formated <- stringr::str_c("View(", selected, ')')
    rstudioapi::sendToConsole(formated, execute = TRUE)
  }
}

#' Format console
#'
#' read console input and output from clipboard, format as script
#'
#' Formated script is written back to clipboard, and inserted to current cursor
#' location
#'
#' @export
#'
format_console <- function(){
  input_lines <- clip_read_lines()
  empty_index <- stringr::str_detect(input_lines, "^\\s*$")
  commands_index <- stringr::str_detect(input_lines, "^\\+|^>")
  input_lines[!(commands_index | empty_index)] <-
    stringr::str_c("# ", input_lines[!(commands_index | empty_index)], sep = "")
  input_lines[commands_index] <-
    stringr::str_replace_all(input_lines[commands_index], "^\\+", " ")
  input_lines[commands_index] <-
    stringr::str_replace_all(input_lines[commands_index], "^>\\s?", "")
  clip_write_lines(input_lines)
  output <- stringr::str_c(input_lines, "\n", collapse = "")
  context <- rstudioapi::getActiveDocumentContext()
  rstudioapi::insertText(context$selection[[1]]$range, output, id = context$id)
}