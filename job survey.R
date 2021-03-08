# RStudio API Code
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(readxl)
library(tm)
library(SnowballC)
library(wordcloud)
library(Hmisc) #for defining errorbar min and max

# Data Import & Cleaning
survey <- read_xlsx("../data/job survey data 1-2.xlsx",
                    col_types = c("date", "date", "text", "text", "numeric", "numeric", "logical", "date", "text", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "text", "text", "text", "text", "numeric", "text", "text", "numeric", "text", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "numeric")) %>%
  mutate_at(vars(filter_alum,	
         filter_io,
         highest_degree), funs(factor(.))
  ) %>%
  mutate(geo = factor(geo, levels = c("DC Metro Area", 
                                      "Completely remote/Tele-work", 
                                      "Urban – moderate/low cost-of-living metropolitan area (e.g., Dallas, Charlotte, Phoenix)", 
                                      "Urban – high cost-of-living metropolitan area other than DC (e.g., NYC, San Francisco)", 
                                      "Rural"),
                      labels = c("DMV", 
                                 "Remote", 
                                 "Urban-low cost", 
                                 "Urban-high cost", 
                                 "Rural"))) %>%
  mutate(negotiate = factor(negotiate, levels = c("0", "1–5%", "6–10%", "11-15%", "16%+"), 
                            labels = c("0", "1–5%", "6–10%", "11-15%", "16%+")),
         change_jobs = factor(change_jobs, levels = c("0", "1–5%", "6–10%", "11-15%", "16%+"), 
                              labels = c("0", "1–5%", "6–10%", "11-15%", "16%+")),
         promotion = factor(promotion, levels = c("0", "1–5%", "6–10%", "11-15%", "16%+"), 
                            labels = c("0", "1–5%", "6–10%", "11-15%", "16%+"))
         ) %>%
  filter(filter_alum == "Yes" & filter_io == "Yes" & highest_degree != "NA") # add `highest_degree != "Other"`
  

## salary cleaning
## got rid of nondigit characters in salary field, converted k to 000, deleted nonnumeric portions of input
## added adjusted salary based on consumer price index for highest degree year

## data not yet cleaned for text processing

# Set theme palette
my_palette <- brewer.pal(8, "Set2")

# Word cloud of Search Terms
## MAKE SURE TO FIRST RUN FIND AND REPLACE: IO and I/o >> I-O; HR and HUMAN RESOURCES >> HUMAN-RESOURCES
search_terms_corpus <- VCorpus(VectorSource(survey$search_terms)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = T) %>%
  tm_map(PlainTextDocument) #%>%
  # tm_map(removeWords, stopwords('english')) #%>%
  # tm_map(stemDocument)
# search_terms_corpus <- Corpus(VectorSource(search_terms_corpus)) %>%
#   tm_map(removeWords, c("character", "list"))
wordcloud(search_terms_corpus, scale = c(2, .25), min.freq = 2, 
          random.order = FALSE, rot.per = 0, fixed.asp = F, colors = brewer.pal(6, "Set2"))

# Most Common Search Terms
TermDocumentMatrix(search_terms_corpus) %>% 
  as.matrix %>% 
  rowSums() %>% 
  sort(decreasing = T) %>% 
  head(n = 10)

# REPLACED BELOW WITH BOX AND WHISKER
# Bar Graph of Salary by Geographical Location and Degree
## computing mean and sd for errorbar
# survey_summary <- survey %>%
#   filter(filter_io != "no") %>%
#   group_by(highest_degree, geo) %>%
#   summarise(mean = mean(first_salary, na.rm = T),
#             sd = sd(first_salary, na.rm = T))
# survey_summary %>% ggplot(aes(x = geo, y = mean, group = highest_degree, fill = highest_degree)) +
#   geom_col(position = "dodge") +
#     labs(title = "Salary by Location and Highest Degree",
#          x = "Geographical Location",
#          y = "Average Salary") +
#   scale_fill_brewer(palette = "Set2")

# survey %>% ggplot(aes(x = geo, y = first_salary, group = highest_degree, fill = highest_degree)) +
#   geom_col(position = "dodge") +
#   stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = 1)) +
#   stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), mapping = aes(group = highest_degree),
#              geom = "errorbar", width = 0.1, position = position_dodge(width = 1)) +
#   labs(title = "Salary by Location and Highest Degree",
#        x = "Geographical Location",
#        y = "Salary")

#
# Job Boards frequency of mention
job_boards_corpus <- VCorpus(VectorSource(survey$job_boards)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = T) %>%
  tm_map(PlainTextDocument)
dtm <- DocumentTermMatrix(job_boards_corpus)
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
names(freq) <- toupper(names(freq))
head(freq) # or just print freq and choose how many to display

# Negotiation response frequencies
ggplot(survey, aes(x = negotiate)) +
  geom_bar(fill = my_palette[1]) +
  labs(title = "How much should new graduates expect to negotiate their first salary?",
       x = "% Increase from Initial Offer",
       y = "Response Frequency")

# Median adj salary by highest degree
ggplot(survey, aes(x = highest_degree, y = adj_salary, fill = highest_degree)) +
  geom_boxplot() +
  scale_fill_brewer(name = "Degree", palette = "Set2") +
  labs(title = "Salary at First I/O Job after Graduation",
       x = "",
       y = "Salary")

# Salary by degree and geo - box and whisker facet wrap
ggplot(survey, aes(x = highest_degree, y = adj_salary, fill = highest_degree)) +
  geom_boxplot() +
  facet_grid(. ~ geo) +
  scale_fill_brewer(name = "Degree", palette = "Set2") +
  labs(title = "Salary at First I/O Job after Graduation",
       x = "",
       y = "Salary")

# Most Common Job Titles - check data; may need to remove punctuation or other text cleaning
## definitely check for HR vs Human Resources and all IO varieties
job_titles_tbl <- survey$first_job_title %>% 
  toupper() %>%
  table() %>%
  sort(decreasing = T)
head(job_titles_tbl)

# Salaries increasing over time?
## plot of adjusted salary over degree year
ggplot(survey, aes(x = degree_year, y = adj_salary)) +
  geom_smooth(method = "lm") +
  labs(title= "Inflation-Adjusted Salaries over Time",
       x = "Graduation Year",
       y = "Salary (Adjusted for Inflation)") +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 100000)) # scales::comma accesses the comma format from the scales library (instead of sci. notation)

## standardized regression of adjusted salary on degree year, controlling for highest degree
lmz <- lm(data = survey, scale(adj_salary) ~ scale(degree_year) + scale(as.numeric(highest_degree)))
summary(lmz)

# salary stats pivot table 
## by degree, overall
stats <- survey %>% 
  group_by(highest_degree) %>%
  summarise(count = n(),
            avg_salary = mean(adj_salary, na.rm = T),
            sd = sd(adj_salary, na.rm = T),
            min = min(adj_salary, na.rm = T),
            max = max(adj_salary, na.rm = T)
  )
## by degree x geo
stats_geo <- survey %>% 
  group_by(highest_degree, geo) %>%
  summarise(count = n(),
            avg_salary = mean(adj_salary, na.rm = T),
            sd = sd(adj_salary, na.rm = T),
            min = min(adj_salary, na.rm = T),
            max = max(adj_salary, na.rm = T)
            )
## combined
stats_combined <- bind_rows(stats, stats_geo) %>%
  arrange(desc(highest_degree)) %>%
  select(highest_degree, geo, count, avg_salary, sd, min, max) %>%
  mutate(geo = as.character(geo)) %>%
  mutate(geo = replace_na(geo, "All Categories"))

  
