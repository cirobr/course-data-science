# dependencies
pkg <- c("e1071",
         "vctrs"
)
install.packages(pkg)

# main packages
pkg <- c("dslabs",          # datasets for the course
         "ggplot2",         # plot data, charts and graphs
         "ggthemes",        # graphic themes for ggplot
         "tidyverse",       # %>%, tidy data, wide data, excel grabber, html grabber, etc
         "gtools",          # probability
         "caret",           # linear regression, machine learning
         "knitr",           # report generator package
         "broom"            # improves do() function: tidy(), glance(), augment()
)
install.packages(pkg)

# data wrangling
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

# machine learning
pkg <- c(#"tidyverse",
         #"caret",
         "randomForest",
         "keras")
install.packages(pkg)
