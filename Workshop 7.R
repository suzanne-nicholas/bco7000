library(tidyverse)
library(skimr)

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

skim(coffee_ratings)

#Option 1 to convert from character to factors - updates dataset to new variable type
coffee_ratings<-coffee_ratings%>%
  mutate(
    species=as_factor(species),
    country_of_origin=as_factor(country_of_origin)
  )


#option 2 - change is made buy only in the output (stays as character in dataset)
coffee_ratings$owner<-as_factor(coffee_ratings$owner)

coffee_ratings%>% count(country_of_origin, sort=TRUE)
coffee_ratings<-coffee_ratings%>%
  mutate(
    country_of_origin_lumped=fct_lump(country_of_origin,n=14))

coffee_ratings%>%count(country_of_origin_lumped, sort=TRUE)

#use ggplot with geom_col where one of the variables is country_of_orgin_lumped and another variable

coffee_ratings%>%ggplot(
  aes(country_of_origin_lumped, number_of_bags)) +
  geom_col()


#convert from nominal to ordinal
coffee_ratings%>%
  mutate(
    country_of_origin_lumped=fct_reorder(country_of_origin_lumped, number_of_bags, sum)
  )%>%ggplot(
    aes(country_of_origin_lumped, number_of_bags)) +
  geom_col() +
  coord_flip()

#let see the distribution total_cup_points

coffee_ratings%>% ggplot(aes(total_cup_points))+
  geom_histogram()






