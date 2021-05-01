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


# 2005 ***************
all_2005 <- year_clean("ENPECYT 2005-2017/2005/")
r1enpecyt_2005 <- all_2005[[1]]
r2enpecyt_2005 <- all_2005[[2]]
r3enpecyt_2005 <- all_2005[[3]]
r4enpecyt_2005 <- all_2005[[4]]
r5enpecyt_2005 <- all_2005[[5]]
r6enpecyt_2005 <- all_2005[[6]]
r7enpecyt_2005 <- all_2005[[7]]
r8enpecyt_2005 <- all_2005[[8]]
r9enpecyt_2005 <- all_2005[[9]]
rAenpecyt_2005 <- all_2005[[10]]
rBenpecyt_2005 <- all_2005[[11]]
rCenpecyt_2005 <- all_2005[[12]]
usethis::use_data(r1enpecyt_2005, overwrite = TRUE)
usethis::use_data(r2enpecyt_2005, overwrite = TRUE)
usethis::use_data(r3enpecyt_2005, overwrite = TRUE)
usethis::use_data(r4enpecyt_2005, overwrite = TRUE)
usethis::use_data(r5enpecyt_2005, overwrite = TRUE)
usethis::use_data(r6enpecyt_2005, overwrite = TRUE)
usethis::use_data(r7enpecyt_2005, overwrite = TRUE)
usethis::use_data(r8enpecyt_2005, overwrite = TRUE)
usethis::use_data(r9enpecyt_2005, overwrite = TRUE)
usethis::use_data(rAenpecyt_2005, overwrite = TRUE)
usethis::use_data(rBenpecyt_2005, overwrite = TRUE)
usethis::use_data(rCenpecyt_2005, overwrite = TRUE)

# 2007 ***************
all_2007 <- year_clean("ENPECYT 2005-2017/2007/")
r1enpecyt_2007 <- all_2007[[1]]
r2enpecyt_2007 <- all_2007[[2]]
r3enpecyt_2007 <- all_2007[[3]]
r4enpecyt_2007 <- all_2007[[4]]
r5enpecyt_2007 <- all_2007[[5]]
r6enpecyt_2007 <- all_2007[[6]]
r7enpecyt_2007 <- all_2007[[7]]
r8enpecyt_2007 <- all_2007[[8]]
r9enpecyt_2007 <- all_2007[[9]]
rAenpecyt_2007 <- all_2007[[10]]
rBenpecyt_2007 <- all_2007[[11]]
usethis::use_data(r1enpecyt_2007, overwrite = TRUE)
usethis::use_data(r2enpecyt_2007, overwrite = TRUE)
usethis::use_data(r3enpecyt_2007, overwrite = TRUE)
usethis::use_data(r4enpecyt_2007, overwrite = TRUE)
usethis::use_data(r5enpecyt_2007, overwrite = TRUE)
usethis::use_data(r6enpecyt_2007, overwrite = TRUE)
usethis::use_data(r7enpecyt_2007, overwrite = TRUE)
usethis::use_data(r8enpecyt_2007, overwrite = TRUE)
usethis::use_data(r9enpecyt_2007, overwrite = TRUE)
usethis::use_data(rAenpecyt_2007, overwrite = TRUE)
usethis::use_data(rBenpecyt_2007, overwrite = TRUE)

# 2009 ***************
all_2009 <- year_clean("ENPECYT 2005-2017/2009/")
enpecyt2009_cb1 <- all_2009[[1]]
enpecyt2009_cb2 <- all_2009[[2]]
enpecyt2009_cs <- all_2009[[3]]
enpecyt2009_vivhog <- all_2009[[4]]
usethis::use_data(enpecyt2009_cb1, overwrite = TRUE)
usethis::use_data(enpecyt2009_cb2, overwrite = TRUE)
usethis::use_data(enpecyt2009_cs, overwrite = TRUE)
usethis::use_data(enpecyt2009_vivhog, overwrite = TRUE)

# 2011 ***************
all_2011 <- year_clean("ENPECYT 2005-2017/2011/")
enpecyt2011_cb1 <- all_2011[[1]]
enpecyt2011_cb2 <- all_2011[[2]]
enpecyt2011_cs <- all_2011[[3]]
enpecyt2011_vivhog <- all_2011[[4]]
usethis::use_data(enpecyt2011_cb1, overwrite = TRUE)
usethis::use_data(enpecyt2011_cb2, overwrite = TRUE)
usethis::use_data(enpecyt2011_cs, overwrite = TRUE)
usethis::use_data(enpecyt2011_vivhog, overwrite = TRUE)

# 2013 ***************
all_2013 <- year_clean("ENPECYT 2005-2017/2013/")
enpecyt2013_cb1 <- all_2013[[1]]
enpecyt2013_cb2 <- all_2013[[2]]
enpecyt2013_cs <- all_2013[[3]]
enpecyt2013_vivhog <- all_2013[[4]]
usethis::use_data(enpecyt2013_cb1, overwrite = TRUE)
usethis::use_data(enpecyt2013_cb2, overwrite = TRUE)
usethis::use_data(enpecyt2013_cs, overwrite = TRUE)
usethis::use_data(enpecyt2013_vivhog, overwrite = TRUE)

# 2015 ***************
all_2015 <- year_clean("ENPECYT 2005-2017/2015/")
enpecyt2015_cb1 <- all_2015[[1]]
enpecyt2015_cb2 <- all_2015[[2]]
enpecyt2015_cs <- all_2015[[3]]
enpecyt2015_vivhog <- all_2015[[4]]
usethis::use_data(enpecyt2015_cb1, overwrite = TRUE)
usethis::use_data(enpecyt2015_cb2, overwrite = TRUE)
usethis::use_data(enpecyt2015_cs, overwrite = TRUE)
usethis::use_data(enpecyt2015_vivhog, overwrite = TRUE)

# 2017 ***************
all_2017 <- year_clean("ENPECYT 2005-2017/2017/")
enpecyt2017_cb1 <- all_2017[[1]]
enpecyt2017_cb2 <- all_2017[[2]]
enpecyt2017_cs <- all_2017[[3]]
enpecyt2017_vivhog <- all_2017[[4]]
usethis::use_data(enpecyt2017_cb1, overwrite = TRUE)
usethis::use_data(enpecyt2017_cb2, overwrite = TRUE)
usethis::use_data(enpecyt2017_cs, overwrite = TRUE)
usethis::use_data(enpecyt2017_vivhog, overwrite = TRUE)

