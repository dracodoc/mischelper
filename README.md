# mischelper
RStudio Addin that collected several miscellaneous helper functions to save some time.

Used to be named [formatpaste](https://github.com/dracodoc/formatpaste).

These functions are very simple in concept but could save some time in life.

## Installation
To install:
* Install RStudio newest release version.
* Run following lines in RStudio console:


        install.packages("devtools")
        devtools::install_github("dracodoc/mischelper")
        
  The packages `stringr`, `stringi`, `magrittr`, `microbenchmark` will be installed if not already available. `microbenchmark` is only required for `benchmark` function, but I chose to install it automatically because it is a simple and small package with little burden to install.
  
### Helper with clipboard
To use the Addin, copy text into clipboard, put cursor to desired position, select function from the drop down list in Addin toolbar button. The formated text will be inserted into current cursor position.

* Unwrap text

  Remove unneeded hard line breaks of text in clipboard, then paste into current cursor position.
* Unwrap with blank line

  Remove hard line breaks, add add extra blank line between paragraphs, then paste into the cursor position.
* Flip windows path

  Convert "\" in clipboard to "/", then paste into current cursor position. Thus windows path can be used in R.

### microbenchmark selected code


## Keyboard shortcuts
You can also assign keyboard shortcut to functions:
* Select `Browse Addins` from the Addin toolbar button.
* Click `Keyboard Shortcuts` in left bottom.
* Click the Shortcut column for each row to assign keyboard shortcut.
