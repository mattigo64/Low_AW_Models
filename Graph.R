library(ggplot2)
g1 = ggplot(data = low_awSal,aes(x=Temperature, y = Rate, group = GrowMeth))+
  geom_point(size = 6)+
  theme(legend.position = 'none')+
  scale_y_continuous(limits = c(-0.6,0.02), breaks = c(0,-0.2,-0.4,-0.6))+
  scale_x_continuous(limits = c(-25,45), breaks = c(-20,-10,0,10,20,30,40))+
  theme(axis.title = element_text( face = 'bold',size =28), axis.text = element_text(size=26, face = 'bold'))+
  theme(plot.title = element_text(size=36))+
  labs(title=expression(~bolditalic('Salmonella')~ ~bold('Survival vs Temperature')), x = 'Temperature (°C)', y = 'Rate (logCFU/g/day)')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#######
g2 = ggplot(data = low_awSal,aes(x=Temperature, y = Rate, group = GrowMeth))+
  geom_point(aes(color=GrowMeth), size =6)+
  theme(legend.position = 'none')+
  scale_y_continuous(limits = c(-0.6,0.02), breaks = c(0,-0.2,-0.4,-0.6))+
  scale_x_continuous(limits = c(-25,45), breaks = c(-20,-10,0,10,20,30,40))+
  theme(axis.title = element_text( face = 'bold',size =28), axis.text = element_text(size=26, face = 'bold'))+
  theme(plot.title = element_text(size=36, face ='bold'))+
  labs(title=expression(~bolditalic('Salmonella')~ ~bold('Survival vs Growth')), x = 'Temperature (°C)', y = 'Rate (logCFU/g/day)')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))


jpeg('Salmonella v Temp', units ='in', width = 10, height = 10, res = 300)
g1
dev.off()

jpeg('Salmonella v Grow', units ='in', width = 10, height = 10, res = 300)
g2
dev.off()

