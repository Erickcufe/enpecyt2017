## code to prepare `DATASET` dataset goes here
cb1_2017 <- read.csv("ENPECYT 2005-2017/2011/enpecyt2011_cb1.csv")

filter_cnames <- function(df){
  cnames <- colnames(df)
  correct <- lapply(cnames, function(x){
    a <- stringr::str_split(x, "[.]")
  })
  c_correct <- lapply(correct, function(x){
    a <- x[[1]][1]
  })
  c_correct <- unlist(c_correct)
  colnames(df) <- c_correct
  return(df)
}

year_clean <- function(directory){
  docs <- list.files(directory, pattern = ".csv")
  a <- lapply(docs, function(x){
    b <- read.csv(paste0(directory,x))
    c <- filter_cnames(b)
  })
}


# 2015

all_2015 <- year_clean("ENPECYT 2005-2017/2005/")

r1enpecyt_2015 <- all_2015[[1]]
r2enpecyt_2015 <- all_2015[[2]]
r3enpecyt_2015 <- all_2015[[3]]
r4enpecyt_2015 <- all_2015[[4]]
r5enpecyt_2015 <- all_2015[[5]]
r6enpecyt_2015 <- all_2015[[6]]
r7enpecyt_2015 <- all_2015[[7]]
r8enpecyt_2015 <- all_2015[[8]]
r9enpecyt_2015 <- all_2015[[9]]
rAenpecyt_2015 <- all_2015[[10]]
rBenpecyt_2015 <- all_2015[[11]]
rCenpecyt_2015 <- all_2015[[12]]

usethis::use_data(r1enpecyt_2015, overwrite = TRUE)
usethis::use_data(r2enpecyt_2015, overwrite = TRUE)
usethis::use_data(r3enpecyt_2015, overwrite = TRUE)
usethis::use_data(r4enpecyt_2015, overwrite = TRUE)
usethis::use_data(r5enpecyt_2015, overwrite = TRUE)
usethis::use_data(r6enpecyt_2015, overwrite = TRUE)
usethis::use_data(r7enpecyt_2015, overwrite = TRUE)
usethis::use_data(r8enpecyt_2015, overwrite = TRUE)
usethis::use_data(r9enpecyt_2015, overwrite = TRUE)
usethis::use_data(rAenpecyt_2015, overwrite = TRUE)
usethis::use_data(rBenpecyt_2015, overwrite = TRUE)
usethis::use_data(rCenpecyt_2015, overwrite = TRUE)

