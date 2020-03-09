Sys.setlocale(category = "LC_ALL", locale = "Polish")
library(dplyr)
library(ggplot2)
grants_larger <- read.csv("./data/grants_larger.csv",
                          encoding = "UTF-8",
                          stringsAsFactors = FALSE)

head(grants_larger)

remove_alpha <- function(x)
  gsub(patter = "[a-z :óę]", replacement = "", x = x, ignore.case = TRUE)


res <- mutate(grants_larger,
       type = strsplit(type, split = "Konkurs: ") %>%
         sapply(function(i) i[2]) %>%
         strsplit(split = "-") %>%
         sapply(first) %>%
         gsub(pattern = "[0-9]", replacement = "", x = .) %>%
         gsub(pattern = "[ ]+$", replacement = "", x = .),
       subpanel = strsplit(subpanel, split = "-") %>%
         sapply(first) %>%
         gsub(pattern = "[ ]+$", replacement = "", x = .)) %>%

mutate_at(c("coinvestigators",
            "duration",
            "budget"), remove_alpha)

ggplot(res, aes(x = duration, y=budget)) + geom_point()