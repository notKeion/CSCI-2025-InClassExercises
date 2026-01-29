# Make a REGEX to match phone numbers in North America
numbers = c("123-456-1234", "12-4-68", "aaa-bbb-cccc")
# \d matches any digit
# {n} exactly n times
str_detect(numbers, pattern="\\d{3}-\\d{3}-\\d{4}")


# Exercises
# 1. How would you match the literal string "'\? How about "$^$"?
test_cases = c(paste0('\"',"\'\\?"), '\"$^$\"', "")
pattern = paste0('\\"',"\\'\\\\\\?")
str_detect(test_cases, pattern) #"'\?

pattern = '\\"\\$\\^\\$\\"'
str_detect(test_cases, pattern) #"$^$"

# 2. Explain why each of these patterns don’t match a \: "\", "\\", "\\\".
# Escape characters just cannot be matched

# 3. Given the corpus of common words in stringr::words, create regular expressions that find all words that:
library(stringr)
test_cases = words

# Start with “y”.
str_detect(test_cases, pattern = "^y")
# Don’t start with “y”.
str_detect(test_cases, pattern = "^[^y]")
# End with “x”.
str_detect(test_cases, pattern = "x$")
# Are exactly three letters long. (Don’t cheat by using str_length()!)
str_detect(test_cases, pattern = "^.{3}$")
# Have seven letters or more.
str_detect(test_cases, pattern = "^.{7,}")
# Contain a vowel-consonant pair.
str_detect(test_cases, pattern = "[aeiou][^aeiou]")
# Contain at least two vowel-consonant pairs in a row.
str_detect(test_cases, pattern = "[aeiou][^aeiou][aeiou][^aeiou]")
# Only consist of repeated vowel-consonant pairs.
str_detect(test_cases, pattern = "^(?:[aeiou][^aeiou])+$")

# 4. Create 11 regular expressions that match the British or American spellings 
# for each of the following words: airplane/aeroplane, aluminum/aluminium, 
# analog/analogue, ass/arse, center/centre, defense/defence, donut/doughnut, 
# gray/grey, modeling/modelling, skeptic/sceptic, summarize/summarise. 
# Try and make the shortest possible regex!

# 5. Switch the first and last letters in words. Which of those strings are still words?
test_cases[test_cases == str_replace(test_cases, "^(.)(.*)(.)$", "\\3\\2\\1")]


