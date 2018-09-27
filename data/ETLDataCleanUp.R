require(readr)
require(plyr)

file_path = "../01 Data/fatal-police-shootings-data.csv"
df <- read.csv(file_path, header=TRUE, stringsAsFactors=FALSE)
df$name <- NULL
names(df)

str(df)
measures <- c("id", "age")
dimensions <- setdiff(names(df), measures)
dimensions

for(n in names(df)) {
  df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= ""))
}

df["state"] <- data.frame(lapply(df["state"], toupper))

df$race <- gsub("W", "WHITE", df$race)
df$race <- gsub("^[H]", "HISPANIC", df$race)
df$race <- gsub("^[B]", "BLACK", df$race)
df$race <- gsub("^[N]", "NATIVE AMERICAN", df$race)
df$race <- gsub("^[A]", "ASIAN", df$race)
df$race <- gsub("^[O]", "OTHER", df$race)
df["race"]

df$gender <- gsub("F", "FEMALE", df$gender)
df$gender <- gsub("^[M]", "MALE", df$gender)
df["gender"]

head(df)

na2emptyString <- function (x) {
  x[is.na(x)] <- ""
  return(x)
}
if(length(dimensions) > 0) {
  for(d in dimensions) {
    # Change NA to the empty string.
    df[d] <- data.frame(lapply(df[d], na2emptyString))
    # Get rid of " and ' in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
    # Change & to and in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
    # Change : to ; in dimensions.
    df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"))
  }
}

na2zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}
if( length(measures) > 1) {
  for(m in measures) {
    print(m)
    df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
    df[m] <- data.frame(lapply(df[m], na2zero))
    df[m] <- data.frame(lapply(df[m], function(x) as.numeric(as.character(x))))
  }
}
str(df)

write.csv(df, gsub("-data", "-cleaned", file_path), row.names=FALSE, na = "")
