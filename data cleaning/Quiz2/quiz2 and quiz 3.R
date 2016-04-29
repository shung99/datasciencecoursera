setwd("C:/Users/schung/Documents/1_Performance and Development/datasciencecoursera/data cleaning/Q2/")
library(httr)
oauth_endpoints("github") 
myapp <- oauth_app("github", "3ab313538c301fa31716", "233ec0ebcdfb48f0bb0027f5c2ffef4df7876024") 
#Use http://localhost:1410 as the callback url 
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp) 
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req) 
content(req)
list(req)
# curl -u Access Token:x-oauth-basic "https://api.github.com/users/jtleek/repos" 
BROWSE("https://api.github.com/users/jtleek/repos",authenticate("Access Token","x-oauth-basic","basic")) 



library(sqldf) 
acs <- read.csv("getdata_data_ss06pid.csv", header=T, sep=",") 
head(acs) 
sqldf("select pwgtp1 from acs where AGEP < 50")
q2<-sqldf("select distinct AGEP from acs")


hurl <- "http://biostat.jhsph.edu/~jleek/contact.html" 
con <- url(hurl) 
htmlCode <- readLines(con) 
close(con) 
sapply(htmlCode[c(10, 20, 30, 100)], nchar) 
nchar(htmlCode[c(10, 20, 30, 100)])




file_name <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for" 
df <- read.fwf(file=file_name,widths=c(-1,9,-5,4,4,-5,4,4,-5,4,4,-5,4,4), skip=4) 
head(df) 
sum(df[, 4]) 



require(data.table)
require(jpeg)


url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
f <- file.path(getwd(), "ss06hid.csv")
download.file(url, f)
dt <- data.table(read.csv(f))
agricultureLogical <- dt$ACR == 3 & dt$AGS == 6
which(agricultureLogical[1:3]

a<-which(dt$ACR == 3 & dt$AGS == 6)
a[1:3]


url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
f <- file.path(getwd(), "jeff.jpg")
download.file(url, f, mode = "wb")
img <- readJPEG(f, native = TRUE)
quantile(img, probs = c(0.3, 0.8))


library(Hmisc)
library(dplyr)

url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
f1 <- file.path(getwd(), "FGDP.csv")
download.file(url1, f1) 
dt1 <- data.table(read.csv(f1,skip=4,nrow=215))
                                               "Long.Name", "gdp"))
dt1<-dt1[X != ""]
dim(dt1)
head(dt1)
dt1 <- dt1[, list(X, X.1, X.3, X.4)]
setnames(dt1, c("X", "X.1", "X.3", "X.4"), c("CountryCode", "rankingGDP", "Long.Name", "gdp"))
                                             
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
f2 <- file.path(getwd(), "FEDSTATS_Country.csv")
download.file(url2, f2)
dt2 <- data.table(read.csv(f2))
head(dt2)
dt <- merge(dt1, dt2, all = TRUE, by = c("CountryCode"))

sum(!is.na(unique(dt$rankingGDP)))

dt[order(rankingGDP, decreasing = TRUE), list(CountryCode, Long.Name.x, Long.Name.y, 
                                              rankingGDP, gdp)][13]


dt[, mean(rankingGDP, na.rm = TRUE), by = Income.Group]

breaks <- quantile(dt$rankingGDP, probs = seq(0, 1, 0.2), na.rm = TRUE)
dt$quantileGDP <- cut(dt$rankingGDP, breaks = breaks)
dt[Income.Group == "Lower middle income", .N, by = c("Income.Group", "quantileGDP")]