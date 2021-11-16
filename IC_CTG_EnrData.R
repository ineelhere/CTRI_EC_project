library(rclinicaltrials)
ncts <- read.csv("NCTs.csv", stringsAsFactors = FALSE)

temp <- NULL
for (i in 1:length(ncts$NCTID)) {
  z <- clinicaltrials_download(query = ncts$NCTID[i], count = 100, include_results = TRUE)
  temp$NCTID[i] <- z$study_information$study_info$nct_id
  temp$EnrollmentAttr[i] <- z$study_information$study_info$enrollment..attrs
  temp$EnrollmwntText[i] <- z$study_information$study_info$enrollment.text
}
write.csv(temp, "IC_CTG_EnrData.csv")
