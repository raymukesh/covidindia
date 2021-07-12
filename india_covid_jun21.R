library(tidyverse)
library(hrbrthemes)
library(extrafont)

font_
getwd()
loadfonts(device = "win")


india <- read_csv("states.csv")
india_state_pop <- read_csv("states_pop19.csv")

india  %>% filter(!State %in% c("India", "State Unassigned")) %>% pivot_longer(3:5, names_to = "names", values_to = "values") %>% 
ggplot(aes(x = as.Date(Date), y= values, color = names)) + geom_line() + 
  facet_wrap(~State, scale = "free_y" , ncol = 3) 


# Renaming the "Dadra and Nagar Haveli and Daman and Diu"
india$State[india$State == "Dadra and Nagar Haveli and Daman and Diu"] <- "Dadra&Nagar-Daman&Diu"


india <- india %>% left_join(india_state_pop, by = c("State"))




india  %>% group_by(State) %>% mutate(newCases = Confirmed - lag(Confirmed, default = first(Confirmed))) %>% ungroup() %>% 
  filter(!State %in% c("India", "State Unassigned")) %>% mutate(positive = (Confirmed/pop_2019)*100) %>% 
  ggplot(aes(x = as.Date(Date), y= positive, fill = State)) + geom_area() + geom_line(aes(color=State)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 month") +
  facet_wrap(facets = ~reorder(State, -positive), ncol = 6) + 
  labs(title = "COVID 19 - Positivity Rate of Indian States", subtitle = "(Data till 30-06-2021)", caption = "Data: covid19india.org | Compiled by: Mukesh Ray") +
  xlab("\nDate") + ylab("Positivity Rate (%)\n") +
  theme_minimal(base_size=12, base_family="Roboto") + 
  theme(plot.title = element_text(size=27, family = "Futura Md BT", face = "bold"),
        plot.subtitle = element_text(size = 16, family = "Futura Md BT"),
        plot.caption = element_text(size = 16, family = "Futura Md BT"),
        strip.text.x = element_text(size = 12, family = "Futura Md BT", face = 'bold'),
        axis.title.x = element_text(color="#993333", size=16,  family = "Futura Md BT", face = "bold"),
        axis.title.y = element_text(color="#993333", size=16,  family = "Futura Md BT", face = "bold"),
        axis.text.x = element_text(size = 11, angle=90, hjust = 1, family = "Futura Md BT"),
        axis.text.y = element_text(size = 12, family = "Futura Md BT"),
        legend.position = "none")

ggsave("india_positve_1.png", height = 10, width = 18, dpi = 300)


## New Cases by Date
india  %>% group_by(State) %>% mutate(newCases = Confirmed - lag(Confirmed, default = first(Confirmed)),
                                      newDeaths = Deceased - lag(Deceased, default = first(Deceased))) %>% 
  filter(!State %in% c("India", "State Unassigned")) %>% 
  ggplot(aes(x = as.Date(Date), y= newDeaths, fill= State)) + geom_area() + geom_line(aes(color=State)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 month") +
  facet_wrap(facets = ~reorder(State, -newCases), scale = "free_y", ncol = 6) + 
  labs(title = "COVID 19 - Positivity Rate of Indian States", subtitle = "(Data till 30-06-2021)", caption = "Data: covid19india.org | Compiled by: Mukesh Ray") +
  xlab("\nDate") + ylab("Positivity Rate (%)\n") +
  theme_minimal(base_size=12, base_family="Roboto") + 
  theme(plot.title = element_text(size=27, family = "Futura Md BT", face = "bold"),
        plot.subtitle = element_text(size = 16, family = "Futura Md BT"),
        plot.caption = element_text(size = 16, family = "Futura Md BT"),
        strip.text.x = element_text(size = 12, family = "Futura Md BT", face = 'bold'),
        axis.title.x = element_text(color="#993333", size=16,  family = "Futura Md BT", face = "bold"),
        axis.title.y = element_text(color="#993333", size=16,  family = "Futura Md BT", face = "bold"),
        axis.text.x = element_text(size = 11, angle=90, hjust = 1, family = "Futura Md BT"),
        axis.text.y = element_text(size = 12, family = "Futura Md BT"),
        legend.position = "none")









india %>% group_by(State) %>% mutate(newCases = Confirmed - lag(Confirmed, default = first(Confirmed))) %>% 
  filter(!State %in% c("India", "State Unassigned")) %>% mutate(Total = sum(newCases))




india  %>% filter(!State %in% c("India", "State Unassigned")) %>% 
  ggplot(aes(x= Date, y=Confirmed, group = 1, na.rm = T)) + scale_x_date(date_labels = "%b%y") + geom_area(aes(fill = State)) +
  geom_line(aes(color = State)) + 
  facet_wrap(facets = ~reorder(State, -Confirmed), ncol = 6) + 
    theme(plot.title = element_text(size=20, family = "Roboto Condensed"),
        axis.title.x = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.title.y = element_text(color="#993333", size=14, family = "Roboto Condensed"),
        axis.text.x = element_text(angle=45, hjust = 1),
        legend.position = "none")
  
 
  
  
  
