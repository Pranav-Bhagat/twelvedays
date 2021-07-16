sing_line <- function(data, num, phrase)
{
  num_word <- data[num,2]
  
  intro <- paste("On the", num_word, "day of Christmas, my true love gave to me,", sep = " ")
  
  phrases <- data %>%
    pull({{phrase}})
  
  phrases[1] <- paste0("and ", phrases[1], ".")
  
  gift_lines <- paste0(phrases[num:1], collapse = ",\n")
  gift_lines <- gsub(" ,", ",", gift_lines)
  
  answer <- paste(intro, gift_lines, sep = "\n")
  return(cat(answer))
}

library(testthat)
xmas <- read.csv("https://www.dropbox.com/s/e584pryn8evm1gz/xmas.csv?dl=1")

test_that("running mean stops when it should", {
  
  expect_error( sing_line(xmas, 7, Full.Phrase))
  
})