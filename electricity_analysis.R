library(tidyverse)
library(ggtext)

#create the dataframe
df <- data.frame(plant= c('Basochhu', 'Chukhha', 'Kurichhu', 'Tala'),
                 capacity = c(64, 336, 60, 1020),
                 plot_cap= c(64+15, 336+25, 60+15, 1020+65),
                 turbines = c(4, 4, 5, 6),
                 width = c(.02, .03, .02, .04),
                 point=c(3.5, 5, 3.5, 9),
                 tur_cap = c(20, 84, 15,170))

df <- df %>% 
  arrange(capacity) %>% 
  mutate(row=row_number())

#for fans
fan <- df %>% 
  select(plant, row, tur_cap, capacity) %>% 
  slice(rep(1:n(), each=2))
fan <- fan %>% 
  mutate(row1=row_number(),
         x = ifelse(row1%%2==0, row - (tur_cap/min(tur_cap)*.05), row + (tur_cap/min(tur_cap))*.05),
         xend = ifelse(row1%%2==0, row + (tur_cap/min(tur_cap)*.05), row - (tur_cap/min(tur_cap))*.05),
         y = ifelse(row1%%2==0, capacity - capacity/min(capacity)*25, capacity + capacity/min(capacity)*25),
         yend = ifelse(row1%%2==0, capacity + capacity/min(capacity)*25, capacity - capacity/min(capacity)*25))


#plotting
df %>% 
  ggplot(aes(row, plot_cap))+
  geom_col(width = c(.02, .02, .03, .07), fill='#f27127')+
  scale_x_continuous(breaks = df$row, labels = df$plant)+
  scale_y_continuous(breaks = seq(0, 1500, 300))+
  geom_segment(data=fan, aes(x=x, y=yend, xend=xend, yend=y), size = c(2, 2, 2, 2, 2.5, 2.5, 5, 5), colour='#f2a516')+
  geom_segment(data=fan, aes(x=x, y=y, xend=xend, yend=yend), size = c(2, 2, 2, 2, 2.5, 2.5, 5, 5), colour='#f2a516')+
  geom_segment(x=3.2, y=1020, xend=4.9, yend=1020, size = 5, colour='#f2a516')+
  geom_point(data = df, aes(x=row, y = capacity), size=df$point, colour='#4ca649')+
  annotate('richtext', x=2, y=1300, label= "<span style = 'color:white; font-size:15pt'> In this data viz </span> 
           <p><p><span style = 'color:white; font-size:15pt'>the height of</span>
           <span style = 'color:#f27127; font-size:15pt'>**Orange Pole**</span>
           <span style = 'color:white; font-size:15pt'>is *Installed Capacity*, </span>
           <p><p><span style = 'color:white; font-size:15pt'>the number of</span>
           <span style = 'color:#f2a516; font-size:15pt'>**Yellow Fans**</span>
           <span style = 'color:white; font-size:15pt'>is *Number of Turbines*, and</span>
           <p><p><span style = 'color:white; font-size:15pt'>the length of</span>
           <span style = 'color:#f2a516; font-size:15pt'>**Yellow Fans**</span>
           <span style = 'color:white; font-size:15pt'>is *Capacity of each Turbine*,</span>", fill=NA)+
  labs(title = 'Hydropower Plants of DGPC',
       x='',
       y='Installed Capacity in MW')+
  theme(panel.background = element_rect(fill='black'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill='black'),
        axis.text = element_text(size=16, colour = '#2694bf'),
        plot.title = element_text(size=23, colour = '#4ca649', hjust=.5),
        axis.title = element_text(size=16, colour = 'white'),
        axis.line = element_line(colour = 'white'))

