

library(tidyverse)
library(here)
library(googlesheets4)
library(ggthemes)
library(ggtext)

# https://calschls.org/reports-data/public-dashboards/secondary-student/

chks <- read_sheet("https://docs.google.com/spreadsheets/d/10sfRl_2r63ScixvKvQr1TcErNkBMRE0NGuhnpxHrBXY/edit#gid=0") %>%
  unnest()


chks9 <- chks %>%
  filter(Grade == 9) %>%
  select(-Identity) %>%
  pivot_wider(names_from = StudentGroup,
              values_from = Percentage) %>%
  unnest() %>%
  mutate(gender.diff = as.numeric(Cisgender) - as.numeric(Transgender), 
         sexual.diff = Straight - GLB) %>%
  arrange(desc(sexual.diff))






ggplot(chks9)+
  geom_segment(aes(x = Straight, y = fct_reorder(Indicator, sexual.diff),
                   yend = Indicator, xend = GLB), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
    geom_point(aes(x = Straight, y = fct_reorder(Indicator, sexual.diff)), color = "light blue", size = 4, show.legend = TRUE) +
geom_point(aes(x = GLB, y = fct_reorder(Indicator, sexual.diff)), color = "light green", size = 4, show.legend = TRUE) +
  geom_text(aes(label = sexual.diff, x = (Straight+GLB) / 2, y = fct_reorder(Indicator, sexual.diff)), 
            color = "grey30",
            size = 2.5) + 
  theme_minimal() + 
  labs(title = "Differences between<span style = 'color:green'> **Gay** </span>and <span style = 'color:blue'>**Straight**</span> student experiences",
       caption = "CHKS 9th grade students in Monterey County 2017-19 \nhttps://calschls.org/reports-data/public-dashboards/secondary-student/ ",
       x = "Percent of Students",
       y = "") + 
  theme(plot.title  = element_markdown(),
        plot.title.position = "plot"
  )


ggsave("Sexuality.png", width = 8, height = 5)


ggplot(chks9)+
  geom_segment(aes(x = Cisgender, y = fct_reorder(Indicator, gender.diff),
                   yend = Indicator, xend = Transgender), #use the $ operator to fetch data from our "Females" tibble
               color = "#aeb6bf",
               size = 4.5, #Note that I sized the segment to fit the points
               alpha = .5) +
  geom_point(aes(x = Cisgender, y = fct_reorder(Indicator, gender.diff)), color = "purple", size = 4, show.legend = TRUE) +
  geom_point(aes(x = Transgender, y = fct_reorder(Indicator, gender.diff)), color = "orange", size = 4, show.legend = TRUE) +
  geom_text(aes(label = gender.diff, x = (Cisgender+Transgender) / 2, y = fct_reorder(Indicator, gender.diff)), 
            color = "grey30",
            size = 2.5) + 
  theme_hc() + 
  labs(title = "Differences between<span style = 'color:orange'> **Transgender** </span>and <span style = 'color:purple'>**Cisgender**</span> student experiences",
       caption = "CHKS 9th grade students in Monterey County 2017-19 \nhttps://calschls.org/reports-data/public-dashboards/secondary-student/ ",
       x = "Percent of Students",
       y = "") + 
  theme(plot.title  = element_markdown(),
        plot.title.position = "plot"
  )
 

ggsave("Gender.png", width = 8, height = 5)
