#/Volumes/Hera/Projects/Habit/mr/QC_alert.R

#ra research project - Piya 

install.packages("bit64")

library(pacman)
library(dplyr)
library(tidyr)
library(readr)

# find all file with framewise_displacement
#.Platform$OS.type in console -- if my computer H:, if rhea /blah 
pathway_fdvalues <- '/Volumes/Hera/Projects/Habit/mr/fmriprep/25.0.0*/sub*/ses*/func/*confounds_timeseries.tsv'

if(.Platform$OS.type == "windows") {
  pathway_fdvalues <- "H:/Projects/Habit/mr/fmriprep/25.0.0*/sub*/ses*/func/*confounds_timeseries.tsv"
}

#sys.glob - matches stars to subjects, session, function, etc
all_confounds <- Sys.glob(pathway_fdvalues)

# read them all in to one gia?nt dataframe
# df <- readr::read_delim(all_confounds[1:2], id="file", 
#                         col_select = "framewise_displacement", 
#                         na = c("", "n/a"))


read_displacement <- function(x) read.table(x, header = TRUE, na.strings = "n/a") %>%
  select(framewise_displacement) %>%
  mutate(file = basename(x))

#read_displacement(all_confounds[2])

displacement_list <- lapply(all_confounds, read_displacement)

df <- bind_rows(displacement_list)
                              #column you're making = thing you're running
df %>% group_by(file) %>% 
  summarise(n=n(), 
            mean_fd = mean(framewise_displacement, na.rm = T), 
            ngt.3 = length(which(framewise_displacement > 0.3)))

df_clean <- df %>%
  mutate(
    lunaid = str_extract(file, "(?<=sub-)\\d{5}"), 
    ses = as.integer(str_extract(file, "(?<=ses-)\\d+")), 
    run = as.integer(str_extract(file, "(?<=run-)\\d+")), 
    run = ifelse(is.na(run), 1, run), #for habit
    task = str_extract(file, "(?<=task-)\\w+"), 
    task = str_extract(file, "(?<=task-)[^_]+")
    )

#NAs are actually okay for once 

mean_fd <- df_clean %>% 
  group_by(lunaid, ses, run, task) %>%
  summarise(mean_fd = mean(framewise_displacement, na.rm = TRUE)) %>%
  ungroup()
  