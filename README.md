## formatpaste

This is a very simple RStudio Addin with 3 functions:
* Unwrap text

  Remove unneeded hard line breaks of text in clipboard, then paste into current cursor position.
* Unwrap with blank line

  Remove hard line breaks, add add extra blank line between paragraphs, then paste into the cursor position.
* Flip windows path

  Convert "\" in clipboard to "/", then paste into current cursor position. Thus windows path can be used in R.

The functions are extremely simple but still save some time in my usage, like copying text from PDF, paste windows path into R script.

To install:
* Install RStudio newest release version.
* Run following lines in RStudio console:


        install.packages("devtools")
        devtools::install_github("dracodoc/formatpaste")
        
  The packages `stringr`, `stringi`, `magrittr` will be installed if not already available.
  
To use the Addin, copy text into clipboard, put cursor to desired position, select function from the drop down list in Addin toolbar button. The formated text will be inserted into current cursor position.

You can also assign keyboard shortcut to functions:
* Select `Browse Addins` from the Addin toolbar button.
* Click `Keyboard Shortcuts` in left bottom.
* Click the Shortcut column for each row to assign keyboard shortcut.
