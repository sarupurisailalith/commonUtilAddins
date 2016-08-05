# Common utility Addins for R Studio
commonUtilAddins is an R package that contains a list of common utility [R Studio Addins](https://rstudio.github.io/rstudioaddins/)
    
* Data frame operations:
    + Merge two data frames - *creates a new data frame object after merging*
    + Convert variable types in a data frame - *creates a new data frame object after conversion*
    

## Installation and Usage

Make sure you have the latest and stable version of [devtools](https://github.com/hadley/devtools), [htmltools](https://github.com/rstudio/htmltools), [shiny](https://github.com/rstudio/shiny) and [miniUI](https://github.com/rstudio/miniUI); then install this package

```r
devtools::install_github("sarupurisailalith/commonUtilAddins")
```
Once the package is installed, addins will be avaiable under the 'Addins' menu in RStudio. 

An other way to launch via executing command in the console is as follows:
```r
RStudioAddins::var_conv()
```



### Acknowledgments

* [R Studio Addins](https://rstudio.github.io/rstudioaddins/) - *helped me get started*



To Contirubute to this package - Fork this repository, make your additions and simply submit a pull request. 
