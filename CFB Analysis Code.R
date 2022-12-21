library(tidyverse)
library(forcats)
library(ggpubr)
cfbcombined <- read.csv("cfbcombined.csv")
colnames(cfbcombined)
as_tibble(cfbcombined)

cfbcombined$year <- as.character(cfbcombined$year)

cfbcombined %>% 
  mutate(team_id=fct_reorder(team_id,win_percent)) %>% 
  ggplot(aes(x=team_id, y=win_percent)) +
  geom_col(mapping=aes(fill=win_percent)) +
  labs(title="Win Percentage of All College Football Teams (2013-2021)",
       x="Team ID",y="Win Percentage",fill="Win Percentage") +
  theme(axis.text.x=element_blank())

cfbcombined %>% 
  ggplot(aes(x=year, y=win_percent)) +
  geom_col(mapping=aes(fill=win_percent)) +
  labs(x="Year", y="Win Percentage", fill="Win Percentage") +
  facet_wrap(~team_name)

cfbcombined %>%
  filter(team_name=="Georgia") %>%
  ggplot(aes(x=year, y=win_percent)) +
  geom_col(mapping=aes(fill=win_percent)) +
  labs(title="Georgia", x="Year", y="Win Percentage", fill="Win Percentage")

ggplot(data=cfbcombined) +
  geom_bar(mapping=aes(x=year,fill=offense_type)) +
  facet_wrap(~offense_type) +
  theme(axis.text.x=element_text(angle=30), legend.position="none") +
  labs(x="Year", y="")

ggplot(data=cfbcombined) +
  geom_bar(mapping=aes(x=offense_type,fill=offense_type)) +
  facet_wrap(~year) +
  theme(axis.text.x=element_text(angle=30)) +
  labs(x="Offense Type", y="")

ggplot(data=cfbcombined) +
  geom_point(mapping=aes(x=off_yards_per_game, y=win_percent, color=offense_type)) +
  labs(x="Offensive YPG", y="Win Percentage", color="Offense Type")

ggplot(data=cfbcombined) +
  geom_point(mapping=aes(x=off_yards_per_game, y=win_percent, color=offense_type)) +
  theme(legend.position="none") +
  labs(x="Offensive YPG", y="Win Percentage", color="Offense Type") +
  facet_wrap(~offense_type)

cfbcombined %>% 
  ggplot(aes(x=avg_turnover_margin_per_game, y=win_percent)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  labs(x="Stat", y="Win Percentage") +
  stat_cor()

cfbcombined %>%
  filter(year!="2020") %>% 
  ggplot(aes(x=kickoff_return_touchdowns, y=win_percent)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  labs(title="Kickoff Return Touchdowns Allowed", x="Return TDs Allowed", y="Win Percentage") +
  stat_cor()


cfbcombinednumeric=cfbcombined %>% 
  select_if(is.numeric) 

cfbcombinedcorrelation <- as.data.frame(cor(cfbcombinednumeric))

winpercentcorrelation<- cfbcombinedcorrelation %>% 
  select('win_percent')

winpercentcorrelation <- winpercentcorrelation %>% 
  arrange(-win_percent)

view(winpercentcorrelation)
