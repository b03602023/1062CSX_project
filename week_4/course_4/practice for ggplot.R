#------week 4 -practice for ggplot--------
library(ggplot2)
ggplot(data = CO2)+
  geom_boxplot(data=CO2, aes(x=conc,
                            y=uptake, colour=Plant))+
  geom_point(data=CO2, aes(x=conc, y=uptake,
                          colour=Plant))+
  stat_smooth(data=CO2, aes(x=conc,y=uptake))
