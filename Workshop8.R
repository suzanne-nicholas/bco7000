library(tidyverse)
library(lubridate)
library(skimr)
theme_set(theme_light())

coffee_ratings<-coffee_ratings%>%
  mutate(coffee_id=row_number()) %>%
  filter(total_cup_points >0)

skim(coffee_ratings)

coffee_ratings %>% count(species, sort=TRUE)
coffee_lumped <- coffee_ratings %>%
  filter(!is.na(variety)) %>%
  mutate(variety = fct_lump(variety, 12), sort=TRUE) #sorts the top 12 of 'variety' and assigns 'other' to rest

skim(coffee_lumped)

coffee_lumped %>%
mutate(variety = fct_reorder(variety, total_cup_points)) %>%
  ggplot(aes(total_cup_points, variety)) +
  geom_boxplot()

coffee_lumped %>%
  ggplot(aes(total_cup_points, fill = variety)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~ variety, scale = "free_y") +
  theme(legend.position = "none")


coffee_ratings %>%
  count(producer, sort = TRUE)


coffee_ratings %>%
  count(company, sort = TRUE)


coffee_ratings %>%
  count(color, sort = TRUE)


coffee_ratings %>%
  count(country = fct_lump(country_of_origin, 12), sort = TRUE) %>%
  filter(!is.na(country)) %>%
  mutate(country = fct_reorder(country, n)) %>%
  ggplot(aes(n, country)) +
  geom_col()


coffee_ratings %>%
  filter(!is.na(country_of_origin)) %>%
  mutate(country = fct_lump(country_of_origin, 12),
         country = fct_reorder(country, total_cup_points)) %>%
  ggplot(aes(total_cup_points, country)) +
  geom_boxplot()


coffee_ratings%>%
ggplot() + 
  geom_bar(mapping = aes(x=country_of_origin),
fill="navy") +
  coord_flip()



coffee_ratings%>%
 # filter(!is.na(certification_body))%>%
  ggplot() + 
  geom_bar(mapping = aes(x=certification_body),
           fill="navy") +
  coord_flip()


coffee_ratings%>%
  # filter(!is.na(certification_body))%>%
  ggplot() + 
  geom_bar(mapping = aes(x=variety),
           fill="navy") +
  coord_flip()

ggplot(data=diamonds) + geom_histogram(mapping = aes(x=carat),
                                       binwidth = .5)

coffee_ratings%>%
  ggplot() + 
  geom_histogram(
    aes(total_cup_points),
           binwidth = .7)

coffee_ratings%>%
  ggplot(aes(x=acidity, y=aftertaste, colour = species)) +
  geom_point(size = 1.5) +
  geom_smooth(method = lm, se=FALSE, colour = "blue")
  
results<-lm(total_cup_points~altitude_mean_meters+acidity, data=coffee_ratings)
summary(results)
