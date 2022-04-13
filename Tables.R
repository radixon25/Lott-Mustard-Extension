
#import data
state_data <-
  read.csv('C:/Users/rasha_um7aj52/OneDrive/Desktop/Lott and Mustard Extension/Raw Data/state_lvl.csv')%>%
  dplyr::filter(1976<year & 1993 > year)

#Table 1 (Create Rollout Group Table)
fil_state <- state_data %>%
  subset(shalll == 1) %>%
  group_by(state)%>%
  summarize(year = min(year))
fil_state <- fil_state[order(fil_state$year), ]
fil_state <- stargazer(fil_state, summary = FALSE)

#Table 2 (Summary Statistics of Crime Groups)
select <- c('ratmur', 'ratvio', 'rataga', 'ratpro','aovio','aomur','aopro','aorap','aorob','aoaga','aobur','aolar','aoaut')
state_means <- aggregate(state_data[select], by = state_data['state'], function(x) c(mean = mean(x), sd = sd(x)))
summary_stats <- stargazer(state_means)
summary_stats


