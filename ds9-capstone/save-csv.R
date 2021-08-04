# write datasets to csv
setwd("~/projects/data-science-course/ds9-capstone")

# main datasets
#edx %>% as.data.frame() %>% write_csv(file = "./dat/edx.csv")
#validation %>% as.data.frame() %>% write_csv(file = "./dat/validation.csv")

# aux datasets
movies <- read_delim("./ml-10M100K/movies.dat", col_names = FALSE, delim = "::")
colnames(movies) <- c("movieId", "title", "genres")
edx2 <- left_join(edx, movies, by = "movieId") %>% 
  select(c("userId", "movieId", "rating", "timestamp", "title.y", "genres.y"))
colnames(edx2) <- c("userId", "movieId", "rating", "timestamp", "title", "genres")
edx2 %>% as.data.frame() %>% write_csv(file = "./dat/edx2.csv")
