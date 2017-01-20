# mischelper
RStudio Addin that collected several miscellaneous helper functions to save some time.

Used to be named [formatpaste](https://github.com/dracodoc/formatpaste).

These functions are very simple in concept but may save you some time.

## Updates

_2016.09.09  Added feature to format console input and output_

_2016.09.17  Used expression name instead of full expression in microbenchmark printout_

_2017.01.19  Added a simple timing menu which is just one time run microbenchmark_

_2017.01.19  I just found the function `readClipboard` only works in windows. So some of the clipboard related functions will not work in Mac. Will check if there is a generic apporach to solve this._



## Installation
To install:
* Install RStudio newest release or preview version. (Version 0.99.903 has a bug to run microbenchmark 3 times. Newer preview version don't have this bug.)
* Run following lines in RStudio console:


        install.packages("devtools")
        devtools::install_github("dracodoc/mischelper")
        
The packages `stringr`, `stringi`, `magrittr`, `microbenchmark`, `profvis` will be installed if not already available. `microbenchmark` is only required for `benchmark` function, and `profvis` is only for `profvis` but I chose to install them automatically because they are small packages without much dependencies. 

Functions can be accessed from the drop down list in Addin toolbar button or with keyboard shortcuts. All functions are prefixed with `Misc`.

If you feel you don't need all the functions and they take too much space in the Addin drop down list, you can prevent some to be registered by RStudio. 
- find the package installation folder with `devtools::inst("mischelper")`.
- edit `rstudio\addins.dcf`, remove the sections you don't need.
- restart R session.

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

