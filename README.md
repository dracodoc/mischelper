# mischelper
RStudio Addin that collected several miscellaneous helper functions to save some time.

Used to be named [formatpaste](https://github.com/dracodoc/formatpaste).

These functions are very simple in concept but may save you some time.

## Installation
To install:
* Install RStudio newest release version.
* Run following lines in RStudio console:


        install.packages("devtools")
        devtools::install_github("dracodoc/mischelper")
        
The packages `stringr`, `stringi`, `magrittr`, `microbenchmark`, `profvis` will be installed if not already available. `microbenchmark` is only required for `benchmark` function, and `profvis` is only for `profvis` but I chose to install them automatically because they are small packages without much dependencies. 

Functions can be accessed from the drop down list in Addin toolbar button or with keyboard shortcuts. All functions are prefixed with `Misc`.

If you feel you don't need all the functions and they take too much space in the Addin drop down list, you can prevent some to be registered by RStudio. 
- find the package installation folder with `devtools::inst("mischelper")`.
- edit `rstudio\addins.dcf`, remove the sections you don't need.
- restart R session.

### benchmark selected code
* Misc - microbenchmark

  Select code to be benchmarked, use keyboard shortcut or toolbar menu. The code will be benchmarked for 10 runs in console. `microbenchmark()` parameters can be changed by recalling history in console then editing the last line.
  Since the source code is not changed, you can continue to edit the code, select different code sections to benchmark, continue working on code without needing to remove bechmark code when you are done with benchmark. 

* Misc - profvis

  Similar to microbenchmark, using [`profvis`](https://github.com/rstudio/profvis) to visualize profiling result. 

Currently the source editor window must be in focus before calling function, i.e. if 
you selected some code in source editor but moved focus to console before calling function, the addin will not work. There is a `getSourceEditorContext()` function in `rstudioapi` to solve this, but it is only available after RStudio version `0.99.1111`, which is only available as preview version. I plan to move to this function in future.

### Helper with clipboard
Copy text into clipboard, put cursor to desired position. Each function will insert formated text to current cursor position. This works in both source editor and console.

* Misc - Unwrap text

  Remove unneeded hard line breaks of text in clipboard, then paste into current cursor position.
* Misc - Unwrap with blank

  Remove hard line breaks, add add extra blank line between paragraphs, then paste into the cursor position.
* Misc - Flip windows path

  Convert "\" in clipboard to "/", then paste into current cursor position. Thus windows path can be used in R.


## Keyboard shortcuts
You can assign keyboard shortcut to functions:
* Select `Browse Addins` from the Addin toolbar button.
* Click `Keyboard Shortcuts` in left bottom.
* Click the Shortcut column for each row to assign keyboard shortcut.
