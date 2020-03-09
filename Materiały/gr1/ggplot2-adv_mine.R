library(dplyr)
library(ggplot2)

grants_larger <- read.csv("./data/grants_larger.csv",
                          encoding = "UTF-8",
                          stringsAsFactors = FALSE)

head(grants_larger)

remove_alpha <- function(x)
  as.numeric(gsub(pattern = "[a-z: óę]", replacement = "", x = x,
       ignore.case = TRUE))

grants_df <- mutate(grants_larger, 
       type = strsplit(type, split = "Konkurs: ") %>% 
         sapply(function(i) i[2]) %>% 
         strsplit(split = "-") %>% 
         sapply(first) %>% 
         gsub(pattern = "[0-9]", replacement = "", x = .) %>% 
         gsub(pattern = "[ ]+$", replacement = "", x = .),
       subpanel = strsplit(subpanel, split = "-") %>% 
         sapply(first) %>% 
         gsub(pattern = "[ ]+$", replacement = "", x = .)) %>% 
  mutate_at(c("coinvestigators", "duration",
              "budget"), remove_alpha)
         

ggplot(grants_df, aes(x = duration, y= budget)) + geom_point()

mutate(grants_df, duration = factor(
  duration, levels = as.character(1L:60))) %>%
  ggplot(aes(x = duration, y = log(budget, base = 10))) +
  geom_boxplot() +
  scale_x_discrete(drop = FALSE)

mutate(grants_df, duration = factor(
  duration, levels = as.character(1L:60))) %>%
ggplot(aes(x = duration, y = budget))+
  geom_boxplot() +
  facet_wrap(~ coinvestigators)


panel_df <- mutate(grants_df, panel = gsub(pattern = "[0-9]",
                               replacement = "",
                               subpanel), panel_color = ifelse(panel == "HS",
                                                               "pink",
                                                               ifelse(panel == "NZ",
                                                                      "green", "lightblue")))


ggplot(panel_df, aes(x = panel, fill = subpanel)) + geom_bar()

ggplot(panel_df, aes(x = subpanel, fill = panel)) + geom_bar()

panel_color <- select(panel_df, subpanel, panel_color) %>%
  unique %>% {
    setNames(.[["panel_color"]], .[["subpanel"]])
  }

ggplot(panel_df, aes(x = panel, fill = subpanel, label = subpanel)) +
  # geom_bar(color = "black", fill = "NA") +
  geom_bar(color = "black") +
  geom_text(stat = "count", position = "stack", vjust = 1.5) +
  scale_fill_manual(values = panel_color, guide = FALSE)

group_by(panel_df, subpanel) %>%
  sumarise(n = length(subpanel)) %>%
  mutate(n_cum = cum_sum(n),
         text_pos = n_cum -0.5 * n) %>%
  ggplot(panel_df, aes(x = panel, y = n, fill = subpanel, label = subpanel)) +
  
  geom_bar(color = "black", stat = "identity") +
  geom_text(stat = "count", position = "stack") +
  scale_fill_manual(values = panel_color, guide = FALSE)
  