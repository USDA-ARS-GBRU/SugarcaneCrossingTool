library(readxl)
X3649_crosses <- read_excel("testdata/3649_crosses.xls")

parents<-unique(c(X3649_crosses$`Female Parent`, X3649_crosses$`Male Parent`))
write.csv(parents, "2022parents.csv")

