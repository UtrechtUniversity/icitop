## Todor package - how to
# Written by Dorien Huijser
# Last edit 2023-12-07
# Source: https://github.com/dokato/todor

#### Dependencies ####
install.packages("todor")
library(todor)

#### Setting up ####
options(todor_patterns = c("FIXME", "TODO", "NOTE"), # Which kinds of notes to detect
        todor_rmd = TRUE, # Also search in markdown files
        todor_rnw = FALSE, # Do not search in Rnw files
        todor_rhtml = FALSE, # Do not search in HTML files
        todor_exclude_r = FALSE, # Do not exclude R files
        todor_exclude_packrat = TRUE, # Exclude packrat directory (whatever that may be)
        todor_extra = c("qmd", "dat") # Also search in qmd and dat files
        )

#### Using Todor ####
# In your code, include note signifiers that you defined in the todor_patterns, e.g,
# NOTE This is a test note
# TODO This is something we still have to do
# FIXME Please fix this specific error

#### Collecting the notes ####
# Collect all notes, to dos, etc. in the R project in the Markers tab of RStudio
todor::todor()

# Collect all notes, to dos, etc. from a single file into a markdown document
notes_object <- todor::todor_file("path_to_file.R",
                                  todo_types = c("FIXME", "TODO", "NOTE"),
                                  output = "markdown")

# Alternatively, you can of course only collect one type of note
notes_object_2 <- todor::todor_file("path_to_file.R",
                                  todo_types = "TODO",
                                  output = "markdown")

# Write notes to a file
cat(notes_object, 
    file = "path/to/your/notes_test.md", # Could also be a .txt of course
    append = TRUE)
