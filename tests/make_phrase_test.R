make_phrase <- function(num, num_word, item, verb, adjective, location) 
{
  item <- replace_na(item, "")
  verb <- replace_na(verb, "")
  adjective <- replace_na(adjective, "")
  location <- replace_na(location, "")
  
  vowels = c("a", "e", "i", "o", "u")
  
  if (num > 1) 
  {
    item <- pluralize_gift(item)
    begword <- num_word
  } else 
  {
    if(sum(substring(item, 1, 1) == vowels) == 1) 
    {
      begword <- "an"
    } 
    else 
    {
      begword <- "a"
    }
  }
  
  ans <- paste(begword, adjective, item, verb, location, sep = " ")
  ans <- gsub("\\s+", " ", ans)
  return(ans)
}
library(testthat)

test_that("running mean stops when it should", {
  
  expect_error( make_phrase(10, "ten", "phone", "run", "", "") )
  
})

