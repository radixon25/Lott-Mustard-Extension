
#import data
state_data <-
  read.csv(here("Raw Data","state_lvl.csv"))%>%
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


#Table 3 (TWFE w/ controls)
full_reg_output <- stargazer(full_lmur,full_lvio,full_laga,full_lpro,full_laut, omit = c('state','year'))

#Table 4 (Bacon Decomp)
for (i in 1:length(b_list)){
  x<- xtable(as.data.frame(b_list[i]))
 print(x)
}

#Event Study Graphs
es_list <- list(es_laga,es_laut,es_lbur,es_llar,es_lmur,es_lpro,es_lrap,es_lvio)
for (i in 1:length(es_list)){
  print(es_list[i])
}





