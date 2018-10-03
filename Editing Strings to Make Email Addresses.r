### This was written to turn quite horrible files with student ID strings into usable email address lists
# The output is within the text, using double hashes. It's hard to follow the data wrangling unless you can see the output.

# Install libraries
require(readxl)
require(stringr)
require(dplyr)
require(beepr)

# set the working directory
setwd("~/Documents/Marking_Lecturing/GTA")

# create the path to the csv file I want to edit
path <- file.path("~", "Documents", "Marking_Lecturing", "GTA", "REGISTERS copy.xlsx")

# make my own object that is the csv sheet I want
# my class is on sheet 7
# the column names and first two rows are useless, so I'll skip them
file <- read_excel(path, sheet = 7, col_names = FALSE, skip = 2)

# have a cheeky look at the data
head(file)
## # A tibble: 6 x 11
##          X__1      X__2   X__3   X__4   X__5   X__6   X__7   X__8   X__9
##         <chr>     <chr> <dttm> <dttm> <dttm> <dttm> <dttm> <dttm> <dttm>
## 1 Joe Bloggs1 1111111/1     NA     NA     NA     NA     NA     NA     NA
## 2 Joe Bloggs2 1111112/1     NA     NA     NA     NA     NA     NA     NA
## 3 Joe Bloggs3 1111113/1     NA     NA     NA     NA     NA     NA     NA
## 4 Joe Bloggs4 1111114/1     NA     NA     NA     NA     NA     NA     NA
## 5 Joe Bloggs5 1111115/1     NA     NA     NA     NA     NA     NA     NA
## 6 Joe Bloggs6 1111116/1     NA     NA     NA     NA     NA     NA     NA
## # ... with 2 more variables: X__10 <dttm>, X__11 <dttm>
Gosh, it doesn’t look great. What you can’t see with head() is that the data is repeated three times. This was so that the excel file could be printed. That ‘/1’ needs to be removed for sure, we need to add an email address string, and there’s a few other things we can do to make it tidy and useful.

# get only the rows I want
file <- file[1:22,]

# delete the last two characters from the string
file$X__2 <- as.numeric(str_sub(file$X__2, 1, str_length(file$X__2)-2))

# create the email string
email <- "@chester.ac.uk"

# join the student number string with email string
file$X__2 <- paste(file$X__2, email, sep = "")

# have a look at what was made
head(file)
## # A tibble: 6 x 11
##          X__1                  X__2   X__3   X__4   X__5   X__6   X__7
##         <chr>                 <chr> <dttm> <dttm> <dttm> <dttm> <dttm>
## 1 Joe Bloggs1 1111111@chester.ac.uk     NA     NA     NA     NA     NA
## 2 Joe Bloggs2 1111112@chester.ac.uk     NA     NA     NA     NA     NA
## 3 Joe Bloggs3 1111113@chester.ac.uk     NA     NA     NA     NA     NA
## 4 Joe Bloggs4 1111114@chester.ac.uk     NA     NA     NA     NA     NA
## 5 Joe Bloggs5 1111115@chester.ac.uk     NA     NA     NA     NA     NA
## 6 Joe Bloggs6 1111116@chester.ac.uk     NA     NA     NA     NA     NA
## # ... with 4 more variables: X__8 <dttm>, X__9 <dttm>, X__10 <dttm>,
## #   X__11 <dttm>
It would also be good to have a column where the emails end with “;”. This means we could copy and paste the column straight into Outlook to email all students.

# create a ";" string
colon <- ";"

# join the email addresses with ";"
file$X__3 <- paste(file$X__2, colon, sep = "")

# label the relevant columns nicely
names(file) <- c("Name", "Student Number", "Copy Column Into Outlook to Email")

# select only the columns I need
file <- file[,1:3]

# view the output
head(file)
## # A tibble: 6 x 3
##          Name      `Student Number` `Copy Column Into Outlook to Email`
##         <chr>                 <chr>                               <chr>
## 1 Joe Bloggs1 1111111@chester.ac.uk              1111111@chester.ac.uk;
## 2 Joe Bloggs2 1111112@chester.ac.uk              1111112@chester.ac.uk;
## 3 Joe Bloggs3 1111113@chester.ac.uk              1111113@chester.ac.uk;
## 4 Joe Bloggs4 1111114@chester.ac.uk              1111114@chester.ac.uk;
## 5 Joe Bloggs5 1111115@chester.ac.uk              1111115@chester.ac.uk;
## 6 Joe Bloggs6 1111116@chester.ac.uk              1111116@chester.ac.uk;
# that is now a really useful excel file

# let's write it to a csv so it can be accessed and shared easily
write.csv(file, "Class Emails.csv")
