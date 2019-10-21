#' Run Shiny app as background job
#'
#' Run current file in source editor as shiny app in background job, open in
#' browser window, live reload with source changes (if you set
#' `options(shiny.autoreload = TRUE)`).
#'
#' @export
#'
run_shiny_as_job <- function() {
  # options(shiny.autoreload = TRUE)
  current_context <- rstudioapi::getSourceEditorContext()
  app_dir <- dirname(current_context$path)
  script_content <- sprintf("shiny::runApp('%s', launch.browser = TRUE)\n",
                            app_dir)
  # use a meaningful file name as this is shown in jobs pane.
  temp_script <- tempfile(fileext = ".R")
  cat(script_content, file = temp_script)
  rstudioapi::jobRunScript(temp_script, name = app_dir, workingDir = app_dir)
}
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

#' profvis selected
#'
#' profvis selected code in console without changing source code. RStudio have a
#' similar builtin menu in editor toolbar, but that only works with .R script,
#' not working in .Rmd or unsaved file.
#'
#' @export
#'
profv <- function(){
  if (!requireNamespace("profvis", quietly = TRUE)) {
      stop("profvis needed but not automatically installed.\nInstall the package with install.packages(\"profvis\")",
           call. = FALSE)
  }
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

#' View selected list with listviewer
#'
#' Select a list, view it with listviewer in viewer pane. This is less relevant
#' now with RStudio data viewer started to support list.
#'
#' @export
view_list <- function(){
  if (!requireNamespace("listviewer", quietly = TRUE)) {
    stop("listviewer needed but not automatically installed.\nInstall the package with install.packages(\"listviewer\")",
         call. = FALSE)
  }
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

#' Open Data Viewer on Selected expression
#'
#' The RStudio Environment pane variable name column could be too narrow for
#' long names, and it can be difficult to identify one among similar names.
#' Sometimes it's also useful to check an filter expression on a data.frame.
#' Select a variable or expression then use this feature to open the data viewer
#' for it. With RStudio Viewer working on list/objects now, this become even
#' more useful.
#'
#' @export

view_current <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  selection_start <- context$selection[[1]]$range$start
  selection_end <- context$selection[[1]]$range$end
  if (any(selection_start != selection_end)) { # text selected
    selected <- context$selection[[1]]$text
    # this will show "View(get(selected))" in viewer, not optimal
    # View(get(selected))
    formated <- stringr::str_c("View(", selected, ')')
    rstudioapi::sendToConsole(formated, execute = TRUE)
  }
}

#' Convert Console Print Out to Script
#'
#' Read console input and output from clipboard, format as script(remove the >
#' prompt, convert output as comments).
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
#' Generate the literal c() of a character vector
#'
#' It's often needed to create a vector literally even the vector can be
#' obtained in code. For example a sbuset of column names can be get with number
#' index, but it's not safe to use number index in code. Use this function to
#' turn the output of number index into literal format, then you can copy the
#' output to code.
#'
#' @param x a vector holding items x1, x2, ...
#'
#' @return string of "c("x1", "x2")"
#' @export
#'
#' @examples
#' printc(names(mtcars)[1:3])
#'
printc <- function(x){
  cat(paste0('c("', paste(x, collapse = '", "'), '")'))
}
# scan external functions ----
# should organize by funs_inside, so we can replace all usage in one run
organize_fun_table <- function(dt) {
  new_dt <- dt[!(fun_inside %in% fun)][, usage := list(list(fun)),
                                       by = fun_inside]
  # data.table order sort by C-locale, which sort capitalized letter first
  unique(new_dt[, .(fun_inside, usage)], by =
           "fun_inside")[base::order(fun_inside)]
}
#' Scan Function Object For External Functions
#'
#' @param fun_obj
#'
#' @return result table
#' @export
#'
#' @examples mischelper::scan_fun(sort)
scan_fun <- function(fun_obj) {
  # function parameter lost its name so have to use generic name
  organize_fun_table(data.table(fun = "fun_obj",
             fun_inside = codetools::findGlobals(
               fun_obj, merge = FALSE)$functions))
}
# code string
scan_string <- function(code_string) {
  temp_fun <- eval(parse(text = paste0("function() {\n", code_string, "\n}")))
  organize_fun_table(data.table(fun = "code_string",
             fun_inside = codetools::findGlobals(
               temp_fun, merge = FALSE)$functions))
}
#' Scan Source File For External Functions
#'
#' The file must be able to be sourced without error to be scanned, so packages
#' or data need to be prepared before scanning.
#'
#' @param code_file The path of source file
#' @param organize If FALSE, return table of `fun`, `fun_inside`; If TRUE, return
#'   table of `fun_inside`, `list of fun`
#'
#' @return Result table
#' @export
#'
scan_file <- function(code_file, organize = TRUE) {
  source(code_file, local = TRUE, chdir = TRUE)
  names_in_fun <- ls(sorted = FALSE)
  funs_in_each_name <- lapply(names_in_fun, function(f_name) {
    obj <- get(f_name, parent.env(environment()))
    if (is.function(obj)) {
      data.table(fun = f_name,
                 fun_inside = codetools::findGlobals(
                   obj, merge = FALSE)$functions)
    }
  })
  res <- rbindlist(funs_in_each_name)
  if (organize) {
    organize_fun_table(res)
  } else {
    res
  }
}
#' Scan External Functions
#'
#' If some code was selected, scan selected code, otherwise scan current file.
#' Result table will also be opened in RStudio data viewer. The current file
#' must be able to be sourced without error to be scanned, so packages or data
#' need to be prepared before scanning.
#'
#' @return A data.table of functions.
#' @export
#'
scan_externals <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  selection_start <- context$selection[[1]]$range$start
  selection_end <- context$selection[[1]]$range$end
  if (any(selection_start != selection_end)) { # text selected
    selected <- context$selection[[1]]$text
    external_funs <- scan_string(selected)
    View(external_funs)
    external_funs
  } else {
    file_path <- rstudioapi::getSourceEditorContext()$path
    external_funs <- scan_file(file_path)
    View(external_funs)
    external_funs
  }
}