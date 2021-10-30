library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(ggbump)
library(scales)
library(gganimate)
library(countrycode)

ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

ultra_rankings%>%
  left_join(race)%>%
  distinct(runner,.keep_all = TRUE)%>%
  mutate(Year=year(date))%>%
  select(runner, nationality)%>%
  group_by(nationality)%>%
  count(runner)%>%
  summarise(Total=sum(n))%>%
  arrange(desc(Total))%>%
  data.frame()%>%
  slice_head(n=10)->bump

ultra_rankings%>%
  left_join(race)%>%
  distinct(runner,.keep_all = TRUE)%>%
  mutate(Year=year(date))%>%
  select(Year,runner, nationality)%>%
  group_by(nationality,Year)%>%
  filter(nationality=="USA"|nationality=="FRA"|nationality=="GBR"|nationality=="JPN"|
           nationality=="ESP")%>%
  count(runner)%>%
  summarise(Total=sum(n))%>%
  data.frame()%>%
  mutate(nationality=(fct_relevel(nationality,levels="ESP", "JPN", "GBR","FRA","USA")))->bump1

countrycode(bump1$nationality%>%unique()%>%
              sort(),
            origin="iso3c",
            destination = "country.name")%>%
  set_names(bump1$nationality %>% unique() %>% sort())->bumpname

bump1%>%
  mutate(bumpname=bumpname[nationality])->bump1


bump1%>%
ggplot(aes(Year, Total, group = bumpname, colour=bumpname)) +
  geom_bump(smooth = 21, size = 1.5, alpha=0.6,lineend = "round")+
  geom_point(aes(fill=bumpname), colour="white", shape=21,size=6,alpha=0.8,show.legend = FALSE)+
  scale_color_manual(values=c("#0b79f9","#de3e53","#c1c9cb","#d3fb89","#b189fb"))+
  scale_fill_manual(values=c("#0b79f9","#de3e53","#c1c9cb","#d3fb89","#b189fb"))+
  scale_y_continuous(limits=c(0,5500),breaks = c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500))+
  scale_x_continuous(limits=c(2012,2021),breaks=c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(face = 2, size=10,color = "white"),
        legend.background = element_rect(fill = "black"),
        legend.key = element_rect(fill = "black"),
        plot.margin = unit(c(.5, 1, .5, 1), "cm"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(face = 2, color = "white"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(face = 2, color = "white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=16, colour = "white", face="bold"),
        plot.subtitle=element_text(size=11, colour="white", margin=margin(b=15)),
        plot.caption=element_text(hjust=0, size=9, colour="white", margin=margin(t=15)))+
  guides(color=guide_legend(override.aes = list(size=4)))+
  labs(title="COUNTRIES WITH THE HIGHEST NUMBER OF ULTRA-RUNNERS",
       subtitle=str_wrap("The below data visualization looks at the top five countries with the highest number of participants in ultra-running events and how the number of participants varied from 2012 to 2021. Cumulatively, the US has the highest number of ultra-runners, followed by France, the United Kingdom, Japan and Spain",165),
       caption = "Data:BjnNowak-Github Repo via Tidy Tuesday | Design: @annapurani93")->bumpplot

ggsave("bumpplot.png",bumpplot,width=14,height=8)

#animation
bumpplot+
  transition_reveal(Year, keep_last = TRUE) +
  shadow_mark()->bumpanim

anim_save("bumpanim.gif",bumpanim, fps = 10, end_pause = 30, width = 950, height=475)
