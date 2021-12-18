library(DBI)
library(readr)

ids = c(0:100000)
counter = 0
htmldb <- dbConnect(RSQLite::SQLite(), "icdb.sqlite")

for (i in ids){
  myurl <- paste("http://ctri.nic.in/Clinicaltrials/pmaindet2.php?trialid=",i)
  myurl = gsub(" ","",myurl)
  filename = paste("files_may/",i,".html")
  download.file(myurl, filename)
  mystring <- read_file(filename)
  mystring=gsub("<br />"," <br /> IC-linebreak ",mystring)
  if(grepl("Invalid Request!!!", mystring, fixed=TRUE)==FALSE){
    htmls <- data.frame(
      trial_id <- toString(i),
      timestamp <- toString(Sys.time()),
      html <- mystring)
    dbWriteTable(htmldb, "htmls", htmls, append = TRUE )
    counter = counter + 1
    print(paste("Count = ", counter,"ID = ",i, "time = ", Sys.time()))
  }
  else{
    file.remove(filename)
  }
  
  
}
dbGetQuery(htmldb, " alter table htmls rename 'trial_id....toString.i.' to 'trial_id'")
dbGetQuery(htmldb, " alter table htmls rename 'timestamp....toString.Sys.time...' to 'timestamp'")
dbGetQuery(htmldb, " alter table htmls rename 'html....mystring' to 'html'")

dbDisconnect("htmldb")

