# mischelper
RStudio Addin that collected several miscellaneous helper functions to save some time.

Used to be named [formatpaste](https://github.com/dracodoc/formatpaste).

These functions are very simple in concept but may save you some time.

## Updates
**2019.12.10**

Removed some package depdencies so that the addin can be installed with minimal requirements, and only install needed packages when the feature requires it.

Sometimes I have some code commented out but want to run it from time to time. For example I often have some test code right after a function definition. This is used in interactive development so not really part of test framework. New feature will take selected commented out lines, remove comments and run in console.

**2019.10.18**

Running Shiny apps from RStudio will see current global environment, this has burned me more than once when my app runs fine in development, but failed after deployed because it was depending some global environment data implicitly. A shiny app should almost always run in clean session as it was supposed to be deployed. There is [a issue here](https://github.com/rstudio/rstudio/issues/5190) documented various similar requests so it is in popular need. 

Since the issue has been around for some time and I want to use this as soon as possible, and the feature is easy enough to implement thanks to RStudio jobs and rstudioapi, I just implement it in `mischelper`. Now you can click a button in addin menu and launch your current Shiny app in background.

Some details about my design choices because my implementation is somewhat different from the methods described in other places:
- One method is to retrieve the random port of launched app then open with RStudio viewer, this also need to translate url for RStudio server. I tried this method then I need to use a fixed port number which is not ideal (otherwise I'm not sure how can I retrieve the port number programmingly), and the app was opened in viewer pane not maximized, which is almost never the user want.
- Instead I just launch the app with system browser, this solve all the problems.
- I also put the app directory as job name to help your identify different jobs.
- If you want the app to auto reload with source changes, you can use the global option `options(shiny.autoreload = TRUE)`. 
- The job never use global environment, use the app dir as working directory, don't return result back to global environment.

**2017.11.21**

Sometimes I need to convert existing code into a package, then I need to find all external functions used in code, import package, and change the function usage into full qualified names. This is a cumbersome task. Previously I have to read code line by line, rely on error message of building package to find the functions.

In reading book `Extending R` I found [`XRtools::makeImports`](https://github.com/johnmchambers/XRtools/blob/master/R/makeImports.R) can scan a package and find all packages need to be imported. This doesn't fit my need exactly because I'm still developing a package, and I want all the external functions not packages. Still the source code helped me a lot to get my current solution:
- With the mischelper addin menu `Scan Externals`, current selected code or current opened file will be scanned, and the result functions table returned.
![external_funs](/inst/demo/external_funs.png)
- Note the code or file must be able to be sourced properly to be checked, so you may need to load packages or prepare data before checking.
- You can also use the exported function `mischelper::scan_fun`, `mischelper::scan_file` directly.

I have another addin `namebrowser` which could scan all installed packages and build a name table of all functions in them. So it's also possible to use that database and give all candidate packages for every external functions found. I'm not sure how many people will want to use that so I didn't do that yet. For now I need to verify each function usage anyway, and with the packages properly loaded, pressing `F1` should tell me the proper package already. If I scan all installed packages, the database may actually give more false positives and make it harder to determine which is the right one.

**2017.06.27**

`printc` will generate the literal `c("a", "b")` format from the vector `c("a", "b")`. For example I want to rename some columns but I don't want to rely on number index which can break later, so I just use `printc(names(mtcars)[1:3])` then copy the result to code.

**2017.06.21**

The RStudio Environment pane variable name column is too narrow for longnames, so it's difficutl to find the right data frame in long list of similar names. Before RStudio make that column adjustable or use shorter summary description columns, you can select the variable and use `view data frame` to open in data viewer.

You can also select an expression (or part of an expression) and use this shortcut to inspect the content. This way you don't need to save it into a temporary variable. It's a great way to quickly explore expressions, and I have found I'm using it many time everyday.

You can assign a keyboard shortcut for it using the toolbar menu `Addins -> Browse addins -> keyboard shortcuts`.

![viewdf](/inst/demo/view_df2.gif)

**2017.04.10**

Added a feature that will read csv format data in clipboard, convert to data frame and open Viewer in RStudio, write markdown table format back to clipboard so you can use it in markdown document. The dataframe itself is also returned.

**2017.04.06**

I just found RStudio have the built-in menu `profvis selected lines` which is same with my `provis` feature. It can be accessed from the `Profile` menu, a wizard toolbar button (which is only available in `.R` file, not in `.Rmd`) or keyboard shortcut `shift-alt-cmd-P` in Mac.

Thus I removed the same feature in my addin, added another feature I found useful: `Render RMarkdown`. 

When you `knit` a `.Rmd` document, all code are evaluated from fresh, independent from the global environment. This follows the reproducible research principles. 

If I do need a strict reproducible report I will knit the complete report, however sometimes I just want a rendered html with some results and plots, and I don't want to run some computational expensive operations again from beginning, like reading a huge dataset from disk and go through every previous step. 

`rmarkdown::render("your_report.Rmd")` will render the document in the global environment, so if you have the data/object exists already it can get the result immedidately. I found this can save a lot of time for me, especially sometimes I need to render several times in editing. 

**2017.02.01**

I use `str()` a lot to inspect lists or objects. In RStudio you can expand a list/object in environment pane which is just `str()` output, however the space is often too limited to get an overview. If running `str()` in console, it could also need quite some scrolling.

At first I tried to convert `str()` output into a nested list and view it with `listviewer`. However I didn't find a good way to show some meta info of an item, which should be at same level of item and attached to the item.

Then I just parse the output and tag each line with tags, generate an html file then styling it with css. To use the feature just select the list/obj in RStudio source editor or console, click addin menu `Misc - View str()`. Or you can call function directly with `mischelper::render_str()`.

![render](/inst/demo/render.gif)

The css styling can be customized easily. Use chrome/firefox developer tool to inspect the opened page, select the item and edit the cooresponding css, and you can see result immediately. Once you are satisfied with the changes, save the change to css in package, which is located in `system.file("css", "str_render.css", package = "mischelper")`.

The `info` label and the name of input object are added for clarity. Since they are not part of original `str()` output, `()` are used to separate them.

Note this only works for list，data frame or objects that have some nested structure. It doesn't work for simple vectors, but there is not much need of a html view for the simple structure anyway.

**2017.01.29** 

Added feature to call [listviewer](https://github.com/timelyportfolio/listviewer) for selected list or object. The listviewer package itself registered an addin menu but I don't like it to be a modal dialog blocking R session, and I don't want to enable the edit mode. I think edit to list should be done in program so it can be tracked.

Just select the list or object then click addin menu or assign a keyboard shortcut.

![listviewer](/inst/demo/listview.gif)

**2017.01.24** Added mac os clipboard functions. Now the package should work in both windows and mac. The functions are also available for use if you want. Check `?mischelper::clip_read_lines`, `?mischelper::clip_write_lines` for more details.
    Update: the functions cannot read all lines in mac. Please use `clipr` package instead.

**2017.01.19** Added a simple timing menu which is just one time run microbenchmark

**2016.09.17** Used expression name instead of full expression in microbenchmark printout

**2016.09.09** Added feature to format console input and output


## Installation
To install:
* Install RStudio newest release or preview version. (Version 0.99.903 has a bug to run microbenchmark 3 times. Newer preview version don't have this bug.)
* Run following lines in RStudio console:


        install.packages("devtools")
        devtools::install_github("dracodoc/mischelper")
        
The packages `stringr`, `stringi`, `magrittr`, `microbenchmark`, `profvis` will be installed if not already available. `microbenchmark` is only required for `benchmark` function, and `profvis` is only for `profvis` but I chose to install them automatically because they are small packages without much dependencies. 

Functions can be accessed from the drop down list in Addin toolbar button or with keyboard shortcuts. All functions are prefixed with `Misc`.

If you feel you don't need all the functions and they take too much space in the Addin drop down list, you can prevent some to be registered by RStudio. 
- edit the control file by
  `file.edit(system.file("rstudio", "addins.dcf", package = "mischelper"))`
- remove the sections you don't need.
- restart R session.
- You can always restore to default by installing the package again if there is something wrong with the edit.

### Keyboard shortcuts
You can assign keyboard shortcut to functions:
* Select `Browse Addins` from the Addin toolbar button.
* Click `Keyboard Shortcuts` in left bottom.
* Click the Shortcut column for each row to assign keyboard shortcut.

## benchmark selected code
![benchmark](/inst/demo/benchmark.gif)

* Misc - microbenchmark

  Select code to be benchmarked, use keyboard shortcut or toolbar menu. The code will be benchmarked for 10 runs in console. `microbenchmark()` parameters can be changed by recalling history in console then editing the last line.
  Since the source code is not changed, you can continue to edit the code, select different code sections to benchmark, continue working on code without needing to remove bechmark code when you are done with benchmark. 

* Misc - profvis

  Similar to microbenchmark, using [`profvis`](https://github.com/rstudio/profvis) to visualize profiling result. 

Currently the source editor window must be in focus before calling function, i.e. if 
you selected some code in source editor but moved focus to console before calling function, the addin will not work. There is a `getSourceEditorContext()` function in `rstudioapi` to solve this, but it is only available after RStudio version `0.99.1111`, which is only available as preview version. I plan to move to this function in future.

## Helper with clipboard
Copy text into clipboard, put cursor to desired position. Each function will insert formated text to current cursor position. This works in both source editor and console.

### Misc - Format console input and output

It's very common to find R code examples copied directly from console, which have format like this:

    > x <- 3
    > switch(x, 2+2, mean(1:10), rnorm(5))
    [1]  2.2903605  2.3271663 -0.7060073  1.3622045 -0.2892720


    > centre <- function(x, type) {
    + switch(type,
    +        mean = mean(x),
    +        median = median(x),
    +        trimmed = mean(x, trim = .1))
    + }
    > x <- rcauchy(10)
    > centre(x, "mean")
    [1] 0.8760325


To run these examples as script, lots of manual edits are needed. Now you can copy them either from some documents or your console, click addin menu `Misc - Format console`, the formated script will be inserted to current cursor position, and written back to clipboard so you can paste to other applications:

    x <- 3
    switch(x, 2+2, mean(1:10), rnorm(5))
    # [1]  2.2903605  2.3271663 -0.7060073  1.3622045 -0.2892720


    centre <- function(x, type) {
      switch(type,
             mean = mean(x),
             median = median(x),
             trimmed = mean(x, trim = .1))
      }
    x <- rcauchy(10)
    centre(x, "mean")
    # [1] 0.8760325

The [formatR](http://yihui.name/formatR/) package have a related feature, though it will only evaluate valid script and put results in comments.


### Misc - Unwrap text

![unwrap](/inst/demo/unwrap.gif)

  Remove unneeded hard line breaks of text in clipboard, then paste into current cursor position.
### Misc - Unwrap with blank

  Remove hard line breaks, add add extra blank line between paragraphs, then paste into the cursor position.
### Misc - Flip windows path

  Convert "\" in clipboard to "/", then paste into current cursor position. Thus windows path can be used in R.

![flip](/inst/demo/flip.gif)

