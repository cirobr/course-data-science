schedule <- data.frame(day = c("Monday", "Tuesday"), 
                       staff = c("Mandy, Chris and Laura", "Steve, Ruth and Frank")
)
schedule

str_split(schedule$staff, ",\\s|\\sand\\s")  #3

str_split(schedule$staff, ", | and ")

tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest()
tidy
