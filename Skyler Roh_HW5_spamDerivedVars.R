# Functions to create the derived variables

isRe = function(msg) {
  # The subject line begins with Re: or Fwd: Re:  ... 
  # Returns TRUE if begins with Re
  subject = gsub("^[[:blank:]]+", "", msg$header["Subject"])
  is.Re = grep("^[Re: | Fwd: Re:]", subject)
  return (length (is.Re) != 0)
}

percentCapitals = function(msg) {
  # The percentage of capital letters in subject
  # Do not count punctuation and numbers in denominator 
  # Returns a numeric of length 1
  capitals = length(gregexpr("[A-Z]", msg$header["Subject"])[[1]])
  letters = length(gregexpr("[A-Za-z]", msg$header["Subject"])[[1]])
  return ((capitals/letters) * 100)
}

fromNumericEnd = function(msg) {
  # The From address ends in a number 
  # for example marty66@aol.com
  # Returns a logical of length 1
  grepl("[[:digit:]]@", msg$header["From"])
}

exclaimCount = function(msg) {
  # The number of exclamation marks in body 
  # Returns a numeric of length 1
  bodysplit = strsplit(msg$body, "")
  exclaimPerLine = sapply(bodysplit, function(x) sum(x == "!"))
  return (totalExclaim = sum(exclaimPerLine))
}

# When ready to create data frame
# run the following code
if (TRUE) {
emailDF = data.frame( 
  sapply(emails, function(x) x$isSpam), 
  sapply(emails, isRe),
  sapply(emails, percentCapitals),
  sapply(emails, fromNumericEnd),
  sapply(emails, exclaimCount))

names(emailDF) = c("isSpam", "isRe", "percentCapitals",
                   "fromNumericEnd", "exclaimCount")

save(emailDF, file = "emailDF.rda")
}
