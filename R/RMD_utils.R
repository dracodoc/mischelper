
#' Show rows with same column value in data.table
#'
#' Data have some key column which is supposed to be unique for every row.
#' However there could be duplicated key column values in data due to various
#' reasons. The rows may have duplicated key column value but different value in
#' other columns. It's beneficial to check this subset. Note the common function
#' of duplicated will only show the extra rows duplicate to the source row,
#' while this function will show all rows having common column value.
#'
#' @param dt data.table
#' @param key_col key column name
#'
#' @return data.table subset of rows with same column values
#' @export
#'
#' @examples
#' dt <- data.table(a = letters, keys = c(1:25, 7))
#' show_duplicates(dt, "keys")
show_duplicates <- function(dt, key_col) {
  dupe_keys <- dt[duplicated(dt[[key_col]])][[key_col]]
  dt[dt[[key_col]] %in% dupe_keys]
}
#' Print vector literal code for a string Vector
#'
#' Sometimes we need to write a vector which values come from data, which means
#' to manually copy values and write into a vector definition. This function
#' will take a vector (often came from data, like all unique values of a
#' category column) and print a vector definition string, then it can be copied
#' to source code.
#'
#' @param x a string vector
#'
#' @export
#'
#' @examples
#' printc(letters[1:3])
printc <- function(x){
  cat(charac(x))
}

#' Convert a string vector into vector literal code
#'
#' Given a string vector, generate a string literal of the vec in vec definition
#' format. This can be used internally in other function or to print the vec
#' definition.
#'
#' @param x a string vector
#'
#' @return vector literal code
#' @export
#'
charac <- function(x) {
  paste0('c("', paste(x, collapse = '", "'), '")')
}

#' Export code chunks in RMD into R source code file
#'
#' After developing code in RMarkdown document, export the code chunks into R
#' source code file. It will also generate a file header in comments to show the
#' command how the file was generated to remind it need to be generated from
#' source RMD instead being updated manually. If you turn on the document
#' outline in RStudio, there will be a table of contents to organize the
#' functions by code chunks.
#'
#' @param rmd_path Source RMD file path
#' @param labels code chunk label vector
#' @param output_path Target R source code file path
#'
#' @export
#'
export_chunks <- function(rmd_path, labels, output_path) {
  # reprint the call expression, need to do before labels get names. we have the value of each parameter, just call them. the line need to be in single line for easier copy (multiple line need # in beginning)
  generating_cmd <- str_c('# export_chunks("', rmd_path, '", ', charac(labels), ', "', output_path, '")')
  file_header <- c("# ******* Auto generated from chunks in RMD, Do not edit manually *******",
                   generating_cmd,
                   "# ***********************************************************************")
  lines <- readLines(rmd_path, encoding = "UTF-8")
  # get all chunk start/end lines. RMD require the symbols start from line beginning. need to escape `, {}
  chunk_start_indice <- str_which(lines, "^\`\`\`\\{r\\s.*\\}")
  chunk_labels <- str_match(lines, "^\`\`\`\\{r\\s(.*)\\}")[, 2] %>%
    # there could be option after , so remove the option part. cannot put indexing part immediately as that will break pipe interpretation
    str_split(",", simplify = TRUE) %>%
    .[, 1]
  chunk_end_indice <- str_which(lines, "^\`\`\`$")
  names(labels) <- labels
  label_start_indice <- map(labels, ~ which(chunk_labels == .))
  if(any(map_lgl(label_start_indice, ~ length(.) == 0))) {
    cat("some chunk not found: \n")
    print(label_start_indice)
  }
  # for every label, get next chunk end indice. findInterval get the index of left side of each label
  label_end_indice <- chunk_end_indice[findInterval(label_start_indice, chunk_end_indice) + 1]
  names(label_end_indice) <- labels
  picked_chunks <- map(labels, ~
                         # add chunk label as header
                         c(str_c("# FROM: {", ., "} ----"),
                           lines[(label_start_indice[[.]] + 1):(label_end_indice[[.]] - 1)]))
  # open connection with specified encoding instead by system option
  output_con <- file(output_path, encoding = "UTF-8")
  writeLines(c(file_header, unlist(picked_chunks)), con = output_con)
  close(output_con)
}
# extract chunks, write to temp file and source it. note this cannot take the chunk with extra options?


#' Source the code chunks into global environment
#'
#' In developing, it's easier to just load specific code chunks into environment
#' in one click by running a command. Otherwise you may need to click multiple
#' times for each code chunk, and Run chunks above in RStudio may not work
#' because the code to load may intervene with other exploration code chunks.
#'
#' @param rmd_path Source RMD file path
#' @param labels code chunk label vector
#'
#' @export
#'
source_chunks <- function(rmd_path, labels) {
  temp_script <- tempfile()
  export_chunks(rmd_path, labels, temp_script)
  source(temp_script)
}
# print info with color, possibly with a note. use simple color name represent a color combination. ideally will want to show color in rmd output, but that's too difficult now, have to use css style which doesn't apply to output easily.
# in console we usually set benchmark to be yellow.
# we can use different note tags to make it easier to separate in html output. also the note will be in color as string, easy to find in code chunk part.


#' Print message and information with color
#'
#' A data processing pipeline may have lots of print out. It will be helpful if
#' some quality checking output can be printed with color so it can be catched
#' easily in long output. This function will print the given object, add some
#' notes if provided in color. Note it also return the input object so it can be
#' used in the middle of a pipe.
#'
#' @param obj obj to print out
#' @param note message
#' @param color
#'
#' @export
#'
#' @examples
#' log_print(3, note = "should not be negative number")
#' log_print(-2 > 0, color = "yellow", note = "should not be negative number")
log_print <- function(obj, note = "", color = "cyan") {
  bg_color_vec <- c(cyan = crayon::bgCyan$white, yellow = crayon::bgYellow)
  color_vec <- c(cyan = crayon::cyan, yellow = crayon::yellow)
  # knit result don't have color, add a tag for easier search
  # the R default print out is easier to read and align, always use that when possible
  # first need to capture print results in string, then cat with color. note should not use background color as it will be hard to read.
  cat(stringr::str_c(bg_color_vec[[color]]("-- [INFO]"), " ", color_vec[[color]](note)),
      capture.output(print(obj)),
      sep = "\n")
  # so pipe can continue
  return(invisible(obj))
}
# log print for information display. some condition are key control metrics, need more obvious way to show it passed or not.
# use ensurer pattern, object pipe to function call, then we can use it with multiple expressions. we can also switch between expression itself, log and check easily
# expression %>% log_print %>% check_print
# note expression need to be in () otherwise the . will pick last item
# 1st expression get result, 2nd check condition, if passed, show ---- and in green. if not passed, show xxxx and in orange.
# need to ignore first parameter for our usage

#' Check condition and print result in color
#'
#' For more critical data quality checks, this can be put in last step of data
#' processing pipeline, do some quality check, and will print green message if
#' check passed, red message if the check failed. It can be used with log_print
#' together to give more detailed information.
#'
#' @param pipe_value Take the object transferred from a pipe expression
#' @param test_expression test the object in test expression
#'
#' @export
#'
#' @examples
#' nrow(mtcars) %>%
#'  log_print("row count should be more than 50") %>%
#'  check_print(. > 50)
check_print <- function(pipe_value, test_expression) {
  if (test_expression) {
            cat(crayon::cyan(glue::glue("---------------------------- Check Passed ----------------------------")))
  } else {
    cat(crayon::bgYellow$red(glue::glue("xxxxxxxxxxxxxxxxxxxxxxxxxxxx Check Failed xxxxxxxxxxxxxxxxxxxxxxxxxxxx")))
  }
}
