library(DBI)
library(stringr)

mydb <- dbConnect(RSQLite::SQLite(),"<filepath>")
df <- dbGetQuery(mydb, "select * from target_sample_size")

a<-df$target_sample_size
x <- str_split_fixed(a,"IC-linebreak",10)
x <- data.frame(x)

df$total_sample_size <- x$X1
df$sample_Size_india <- x$X2
df$final_enrollment_numbers_achieved_total <- x$X3
df$final_enrollment_numbers_achieved_india <- x$X4

df[] <- lapply(df, gsub, pattern='Total Sample Size=', replacement='')
df[] <- lapply(df, gsub, pattern='Sample Size from India=', replacement='')
df[] <- lapply(df, gsub, pattern='"', replacement='')

write.csv(df, "df.csv")

df2 <- read.csv("df.csv")

mydb2 <- dbConnect(RSQLite::SQLite(), "test.sqlite")
dbWriteTable(mydb2, "sample_size", df2)

df3 <- dbGetQuery(mydb, "select * from recruitment" )
dbWriteTable(mydb2, "recruitment", df3)

df4 <- dbGetQuery(mydb, "select trial_id, ctri_number, date_of_first_enrollment_global, date_of_first_enrollment_india from dates")             
dbWriteTable(mydb2, "enrollment", df4)

dbDisconnect(mydb2)
