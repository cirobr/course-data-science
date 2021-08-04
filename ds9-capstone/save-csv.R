# write datasets to csv
setwd("~/projects/data-science-course/ds9-capstone")

# main datasets
edx %>% as.data.frame() %>% write_csv(file = "./dat/edx.csv")
validation %>% as.data.frame() %>% write_csv(file = "./dat/validation.csv")
