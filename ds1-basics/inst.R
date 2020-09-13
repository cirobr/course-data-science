# install dependencies
pkg <- c("e1071",
         "vctrs"
)
install.packages(pkg)


# install packages for a fresh start
pkg <- c("dslabs",          # datasets for the course
         "ggplot2",         # plot data, charts and graphs
         "ggthemes",        # graphic themes for ggplot
         "tidyverse",       # %>%, tidy data, wide data, excel grabber, html grabber, etc
         "gtools",          # probability
         "caret",           # linear regression
         "knitr",           # report generator package
         "broom"            # improves do() function: tidy(), glance(), augment()
)
install.packages(pkg)


# data wrangling packages
pkg <- c("dplyr",           # contains tidyverse
         #"tidyverse",       # readr, rvest included
         "readxl",
         "tidyr",
         "htmlwidgets"      # required by str_view()
)
install.packages(pkg)


# matrices
pkg <- c("matrixStats")
install.packages(pkg)


# machine learning important packages
pkg <- c(#"tidyverse",
         #"caret",
         "randomForest")
install.packages(pkg)
