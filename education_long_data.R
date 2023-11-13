
library(tidyverse)

install.pack

# Read-stata-data ---------------------------------------------------------

library(haven)

pidf <- read_dta("data/education_long.dta")

saveRDS(pidf, "data/pidf.rds") ## See size of file

readRDS("data/pidf.rds")

pidf |> dim()

pidf |> glimpse()

pidf |> select(-where(~all(is.na(.))))

pidf |> distinct(year) |> arrange(year)

pidf |> distinct(dataset)

pidf |> group_by(dataset) |> distinct(year) |> arrange(dataset)

pidf |> filter(dataset=='pslm'| dataset=='hies') |> 
  distinct(year) |> arrange(year) 

pidf  |> haven::as_factor() |> filter(dataset=='pslm'| dataset=='hies',district=='JEHLUM') |>
  distinct(year)


pidf  |> haven::as_factor() |> filter(dataset=='pslm'| dataset=='hies',
                                      district=='JEHLUM') |>
  group_by(year) |> summarise(avg=mean(point_estimate, na.rm=TRUE))



pidf  |> haven::as_factor() |> filter(dataset=='pslm'| dataset=='hies',
                                      district=='CHAKWAL') |>
  group_by(year) |> summarise(avg=mean(point_estimate, na.rm=TRUE))





# SWABI ------------------------------------------------------------------



pidf  |> haven::as_factor() |> filter(dataset=='pslm'| dataset=='hies',district=='SWABI', age_range=='5 to 10') |>
  group_by(year) |> summarise(avg=mean(point_estimate, na.rm=TRUE))



# RAJANPUR ----------------------------------------------------------------


pidf  |> haven::as_factor() |> filter(dataset=='pslm'| dataset=='hies',
                                      district=='RAJANPUR', age_range=='5 to 10') |>
  group_by(year) |> summarise(avg=mean(point_estimate, na.rm=TRUE))



# LAYYAH ------------------------------------------------------------------

pidf  |> haven::as_factor() |> filter(dataset=='pslm'| dataset=='hies',
                                      district=='LAYYAH', age_range=='5 to 10') |>
  group_by(year) |> summarise(avg=mean(point_estimate, na.rm=TRUE))



Layyah <- pidf  |> haven::as_factor() |> filter(dataset=='pslm'| dataset=='hies',
                                      district=='LAYYAH', age_range=='5 to 10') |>
  group_by(year) |> summarise(avg=mean(point_estimate, na.rm=TRUE)) |> mutate(ave=round(avg*100,2)) |> 
  ggplot()+aes(x=factor(year),y=ave)+ geom_text(aes(label=ave),vjust=-0.5)+
  geom_bar(stat='identity', fill='steelblue') + labs(x="",y='', title='Average percentage of chilren in school in districtrict Layyah')+theme_minimal()


Rajanpur <- pidf  |> haven::as_factor() |> filter(dataset=='pslm'| dataset=='hies',
                                                district=='RAJANPUR', age_range=='5 to 10') |>
  group_by(year) |> summarise(avg=mean(point_estimate, na.rm=TRUE)) |> mutate(ave=round(avg*100,2)) |> 
  ggplot()+aes(x=factor(year),y=ave)+ geom_text(aes(label=ave),vjust=-0.5)+
  geom_bar(stat='identity', fill='steelblue') + labs(x="",y='', title='Average percentage of chilren in school in districtrict Layyah')+theme_minimal()

library(patchwork)


Layyah+Rajanpur


pidf  |> haven::as_factor() |> filter(dataset=='pslm'| dataset=='hies',
                                      district=='LAYYAH', age_range=='5 to 10') |>
  group_by(year, dimension_level %in% c('Combined','Boy','Girl')) |> summarise(avg=mean(point_estimate, na.rm=TRUE)) |> mutate(ave=round(avg*100,2))




# But-wait ----------------------------------------------------------------


pidf |> haven::as_factor() -> pidf



layyah <- pidf |>   filter(dataset=='pslm'| dataset=='hies',
                 district=='LAYYAH', age_range=='5 to 10', dimension_level== "Combined")|> 
  group_by(year) |> summarise(avg=mean(point_estimate, na.rm=TRUE)) |> mutate(ave=round(avg*100,2)) |> 
  ggplot()+aes(x=factor(year),y=ave)+ geom_text(aes(label=ave),vjust=-0.5)+scale_y_continuous(limits = c(0,80))+
  geom_bar(stat='identity', fill='steelblue') +theme_minimal()+labs(x=NULL,y=NULL)

  # labs(x="",y='', title='Average percentage of chilren in school in districtrict Layyah')+theme_minimal()+
  # 




male_layyah <- pidf |>   filter(dataset=='pslm'| dataset=='hies',
                 district=='LAYYAH', age_range=='5 to 10', dimension_level== "Boy")|> 
  group_by(year) |> summarise(avg=mean(point_estimate, na.rm=TRUE)) |> mutate(ave=round(avg*100,2)) |> 
  ggplot()+aes(x=factor(year),y=ave)+ geom_text(aes(label=ave),vjust=-0.5)+scale_y_continuous(limits = c(0,80))+
  geom_bar(stat='identity', fill='steelblue') +theme_minimal()+labs(x=NULL,y=NULL)


# + labs(x="",y='', title='Average percentage of chilren in school in districtrict Layyah')+theme_minimal()



female_layyah <- pidf |>   filter(dataset=='pslm'| dataset=='hies',
                 district=='LAYYAH', age_range=='5 to 10', dimension_level== "Girl")|> 
  group_by(year) |> summarise(avg=mean(point_estimate, na.rm=TRUE)) |> mutate(ave=round(avg*100,2)) |> 
  ggplot()+aes(x=factor(year),y=ave)+ geom_text(aes(label=ave),vjust=-0.5)+scale_y_continuous(limits = c(0,80))+
  geom_bar(stat='identity', fill='steelblue')+theme_minimal()+labs(x=NULL,y=NULL)

# + labs(x="",y='', title='Average percentage of chilren in school in districtrict Layyah')+theme_minimal()



library(patchwork)

layyah+male_layyah+female_layyah+plot_annotation(title = "Average percentage of chilren in school in districtrict Layyah")



pidf |>   filter(dataset=='pslm'| dataset=='hies',
                 district=='LAYYAH', age_range=='5 to 10', dimension_level %in% c("Urban", "Rural"))|> 
  group_by(year, dimension_level) |> summarise(avg=mean(point_estimate, na.rm=TRUE)) |> mutate(ave=round(avg*100,2))


pidf |>   filter(dataset=='pslm'| dataset=='hies',
                 district=='LAYYAH', age_range=='5 to 10', dimension_level %in% c("Urban", "Rural"))|> 
  group_by(year, dimension_level) |> summarise(avg=mean(point_estimate, na.rm=TRUE)) |> mutate(ave=round(avg*100,2)) |> 
  ggplot() + aes(x=factor(year),y=ave, fill=dimension_level)+
  geom_bar(stat = 'identity', position = position_dodge()) +
  labs(x = "Year", y = "Average Value") +
  theme_minimal()





pidf |>   filter(dataset=='pslm'| dataset=='hies',
                 district=='LAYYAH', age_range=='5 to 10', dimension_level %in% c("Urban", "Rural"))|> 
  group_by(year, dimension_level) |> summarise(avg=mean(point_estimate, na.rm=TRUE)) |> mutate(ave=round(avg*100,2)) |> 
ggplot()+ aes(x = factor(year), y = ave, color = dimension_level) +
  geom_line(linewidth = 2) +
    geom_segment(aes(xend = factor(year), yend = ave, color = dimension_level),
               linewidth=3, lineend = "round") + coord_flip() + theme_minimal() +
geom_line(aes(fill = dimension_level), linewidth = 2, linetype = "dashed")+
  labs(x = "Year", y = "Average Value")+coord_flip()


pidf |>   filter(dataset=='pslm'| dataset=='hies',
                 district=='LAYYAH', age_range=='5 to 10', dimension_level %in% c("Urban", "Rural"))|> 
  group_by(year, dimension_level) |> summarise(avg=mean(point_estimate, na.rm=TRUE)) |> mutate(ave=round(avg*100,2)) |> 
  ggplot() + aes(x=ave,y=year,col=dimension_level)+
  geom_point(size=4)+
  #scale_color_manual(values = color_palette)+
  theme_minimal(base_size = 16,base_family = "Merriweather")+
  theme(legend.position = "none")




# Wealth_quintiles --------------------------------------------------------

pidf |> haven::as_factor() |> group_by(dimension_level) |> summarise(mean=mean(point_estimate))


pidf |> haven::as_factor() |> group_by(dimension_level) |> summarise(mean=mean(point_estimate,na.rm=TRUE))



## Now by wealth quintiles

pidf |> filter(dimension_level %in%c('wq1','wq2','wq3','wq4','wq5'),dataset=='pslm'| dataset=='hies',
                age_range=='5 to 10') |> group_by(year, dimension_level) |>
  summarise(avg=mean(point_estimate, na.rm=TRUE)) |> 
  mutate(ave=round(avg*100,2)) |> na.omit()


## Plot this data

wq_df <- pidf |> filter(dimension_level %in%c('wq1','wq2','wq3','wq4','wq5'),dataset=='pslm'| dataset=='hies',
               age_range=='5 to 10') |> group_by(year, dimension_level) |>
  summarise(avg=mean(point_estimate, na.rm=TRUE)) |> 
  mutate(ave=round(avg*100,2)) |> na.omit()

ggplot(wq_df)+
  aes(x=factor(year),y=ave)+ geom_bar(stat='identity',position = position_dodge())


ggplot(wq_df)+
  aes(x=factor(year),y=ave, fill=dimension_level)+ 
  geom_bar(stat='identity',position = position_dodge())+coord_flip()+
  facet_grid(rows = vars(dimension_level))

ggplot(wq_df)+
  aes(x=factor(year),y=ave, fill=dimension_level)+ 
  geom_bar(stat='identity',position = position_dodge())+coord_flip()+
  facet_wrap(~ dimension_level, scales = "free_y", nrow = 2)+
  theme_minimal()+guides(fill = FALSE)


# Add labels and remove axis values ---------------------------------------



ggplot(wq_df) +
  aes(x = factor(year), y = ave, fill = dimension_level, label = ave) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_text(position = position_dodge(width = 0.9), hjust = -0.3, size = 3) +
  coord_flip() +
  facet_wrap(~ dimension_level, scales = "free_y", nrow = 2) +
  theme_minimal() +
  guides(fill = FALSE) +
  #theme(axis.text = element_blank(), axis.ticks = element_blank())+
  labs(x=NULL,y=NULL,title="Average Value of Children in school by Wealth Quintile",
       subtitle="Pakistan 2004-2018")


# Change the labels on the x axis -----------------------------------------

library(ggplot2)

# Example changing labels for the year column 

wq_df$year <- factor(wq_df$year, levels = unique(wq_df$year), labels = c("2008", "2009", "2010", "2011", "2012", "2013", "2014"))

ggplot(wq_df) +
  aes(x = year, y = ave, fill = dimension_level, label = ave) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_text(position = position_dodge(width = 0.9), hjust = -0.3, size = 3) +
  coord_flip() +
  facet_wrap(~ dimension_level, scales = "free_y", nrow = 2) +
  theme_minimal() +
  guides(fill = FALSE) +
  labs(x = NULL, y = NULL, title = "Average Value of Children in school by Wealth Quintile", subtitle = "Pakistan 2004-2018")


ggplot(wq_df)+
  aes(x=factor(year),y=ave, fill=dimension_level)+ geom_bar(stat='identity',position = position_dodge())+
  theme_minimal()

colors <- thematic::okabe_ito(4)



wq_df |> 
  ggplot(aes(x=factor(year),y = ave, fill = dimension_level)) +
  geom_bar(stat = 'identity') + geom_text(aes(label=ave), hjust=-0.5)+
  geom_vline(xintercept = 0) +
  theme_minimal(base_size = 20, base_family = 'Source Sans Pro') +
  scale_fill_manual(values = colors[1:5]) +
  facet_wrap(vars(dimension_level)) +
  labs(x = element_blank(), y = element_blank()) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = 'none'
  )+ coord_flip()




wq_df |> 
  ggplot(aes(x=factor(year),y = ave, fill = dimension_level)) +
  geom_bar(stat = 'identity') + geom_text(aes(label=ave), hjust=-0.5)+
  geom_vline(xintercept = 0) +
  theme_minimal(base_size = 20, base_family = 'Source Sans Pro') +
  scale_fill_manual(values = colors[1:5]) +
  facet_wrap(vars(dimension_level)) +
  labs(x = element_blank(), y = element_blank()) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = 'none'
  )+ coord_flip() + labs(title = "Percent of students attending school aged 5-10 by Wealth Quintile",
                         subtitle = "Parents are allocated at random and children born to poor parents are less likely to attend school")


wq_df |> 
  ggplot(aes(x=factor(year),y = ave, fill = dimension_level)) +
  geom_bar(stat = 'identity') + geom_text(aes(label=ave), hjust=-0.5)+
  geom_vline(xintercept = 0) +
  theme_minimal(base_size = 20, base_family = 'Source Sans Pro') +
  scale_fill_manual(values = colors[1:5]) +
  facet_wrap(vars(dimension_level)) +
  labs(x = element_blank(), y = element_blank()) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = 'none'
  )+ coord_flip() + labs(title = "Percent of students attending school aged 5-10 by Wealth Quintile",
                         subtitle = "Parents are allocated at random and children born to poor parents are less likely to attend school")+
  labs(caption = "By Zahid, source:PSLM ,PBS")


