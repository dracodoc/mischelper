#' Render str() output in html with style
#'
#' If the output of \code{str()} is long, user can easily get lost in scrolling.
#' This function convert output into html with tag classes and render it with
#' style. You can customize the style by experimenting with chrome/firefox
#' developer tools first, then save to css. The css is located in
#' \code{system.file("css", "str_render.css", package = "mischelper")}
#'
#' By default a browser window will be opened because it is intended for long
#' output. The \code{info} label and the name of input object are added for clarity. Since they are not part of original \code{str()} output, \code{(info)} are used.
#'
#' @param obj the object/variable to be inspected
#'
#' @export
#' @import data.table stringr
render_str <- function(obj) {
  str_lines <- capture.output(str(obj))
  lines_dt <- data.table(line = str_lines)
  lines_dt[, depth := str_count(str_extract(line, "^[\\.\\s]*"), "\\.\\.")]
  lines_dt[, row_no := .I]
  # symbols work as type identifier only in leading position
  lines_dt[str_detect(line, "^[^.\\s]"), type := "info"]
  lines_dt[str_detect(line, "[.\\s]*\\$"), type := "list"]
  lines_dt[str_detect(line, "[.\\s]*@"), type := "slot"]
  lines_dt[str_detect(line, "[.\\s]*- attr"), type := "attribute"]
  # key of info and obj is added by my program, not original output of str()
  # thus use () to seperate. sometimes there are slot name as info. this can separate them.
  lines_dt[type == "info", c("key", "value") :=
             list("(info)", line), by = row_no]
  # the : in "key:value"" : can be matched to : in "int [1:2]"
  lines_dt[type == "list", c("key", "value") :=
             as.list(str_match(line, ".*(\\$\\s[^:]*):(.*)"))[2:3],
           by = row_no]
  lines_dt[type == "slot", c("key", "value") :=
             as.list(str_match(line, ".*(@\\s[^:]*):(.*)"))[2:3],
           by = row_no]
  lines_dt[type == "attribute", c("key", "value") :=
             as.list(str_match(line, ".*(-\\sattr.*)=(.*)"))[2:3],
           by = row_no]
  # trim spaces. esp the slot part have some spaces after key
  lines_dt[, key := str_trim(key)]
  lines_dt[, value := str_trim(value)]
  # top line can get same treament with info, just use different key. no need for special css class since it is the first line anyway.
  # need to get obj name outside of data.table, otherwise it will look at another obj
  obj_name <- deparse(substitute(obj))
  lines_dt[1, key := paste0("(", obj_name, ")")]
  # add some item specific class
  lines_dt[, item_class := paste0("str__", type, " depth_", depth)]
  lines_dt[, value_class := paste0("str__value ",
                                   str_trim(str_extract(value, "^\\s?[[:word:]]*")))]
  # ui ----
  ui <- miniUI::miniPage(
    shiny::includeCSS(system.file("css", "str_render.css", package = "mischelper")),
    miniUI::gadgetTitleBar(paste0("html view of str(", deparse(substitute(obj)), ")")),
    miniUI::miniContentPanel(
      shiny::htmlOutput("main")
    )
  )
  # server ----
  server <- function(input, output, session) {
    ## Your reactive logic goes here.
    getPage <- function() {
      # return(includeHTML("str_buff.html"))
      tag_list <- vector("list", length = nrow(lines_dt))
      for (i in 1:nrow(lines_dt)) {
        tag_list[[i]] <- shiny::tags$div(class = lines_dt[i, item_class],
                            shiny::tags$span(lines_dt[i, key], class = "str__key"),
                            shiny::tags$span(" : ", class = "str__key_value_sp"),
                            shiny::tags$span(class = lines_dt[i, value_class],
                                                              lines_dt[i, value]))
      }
      return(tag_list)
    }
    output$main <- shiny::renderUI({getPage()})
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }
  # run in browser since the main goal is to view bigger output
  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}

#' View obj/variable \code{str()} output in html view by addin
#'
#' @export
render_addin <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  selection_start <- context$selection[[1]]$range$start
  selection_end <- context$selection[[1]]$range$end
  if (any(selection_start != selection_end)) { # text selected
    selected <- context$selection[[1]]$text
    cmd <- stringr::str_c("mischelper::render_str(", selected, ")")
    rstudioapi::sendToConsole(cmd, execute = TRUE)
  }
}

