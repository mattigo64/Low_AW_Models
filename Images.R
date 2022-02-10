##Images
library(ggplot2)
library(lattice)
jpeg('sal~temp', units ='in', width = 5, height = 5, res = 300)
xyplot(Rate ~ Temperature, data = low_awSal, main = "Salmonella Rate of Reduction vs Temperature", ylab = "Rate (logCFU/day/g)",
       panel = function(x,y){
         panel.xyplot(x,y, col = 'black')
         panel.abline(lm(y~x),
                      col = 'red', lwd  =2)
       })


dev.off()

jpeg('sal~aw', units ='in', width = 5, height = 5, res = 300)
xyplot(low_awSal$Rate~low_awSal$Aw, main = "          Salmonella Rate of Reduction 
         vs Water Activity", ylab = "Rate (logCFU/day/g)", xlab = "Water Activity",
       panel = function(x,y){
         panel.xyplot(x,y, col = 'black')
         panel.abline(lm(y~x),
                      col = 'red', lwd  =2)
       })
dev.off()



library(lattice)
low_awSal$ST = ifelse(low_awSal$Serovar == 'Salmonella Enteritidis PT 30 (ATCC BAA-1045)', 1, 
                      ifelse(low_awSal$Serovar == 'Salmonella Typhimurium Non-DT104 Cocktail',2,
                             ifelse(low_awSal$Serovar == 'Salmonella oranienburg2',3, 
                                    ifelse (low_awSal$Serovar == 'Salmonella poona',4,0 ))))
jpeg('sal~strains', units = 'in', width = 5, height =5, res =300)
xyplot(Rate~Temperature, groups =  ST, data = low_awSal,cex = 1, pch = c(1,1,1,1,1), col = c('black', 'royalblue2', ' forestgreen', 'sienna2', 'slategray3'), main = '          Salmonella Rate of Reduction vs 
          Temperature with Serovar', ylab = 'Rate (log CFU/day/g)')
dev.off()

jpeg('sal, wet~dry', units = 'in', width = 5, height =5, res =300)
low_awSal$InocMeth = ifelse(low_awSal$Inoculation =="Dry",1,0)
xyplot(Rate~Temperature, groups =  InocMeth, data = low_awSal,cex = 1, pch = c(15,1), col = c('grey', 'black'),main = "Salmonella Wet vs Dry Innoculation Methods")
dev.off()


jpeg('sal, wet~dry', units = 'in', width = 5, height =5, res =300)
low_awSal$Length = ifelse(low_awSal$Time <=30,1,0)
xyplot(Rate~Temperature, groups =  Length, data = low_awSal,cex = 1, pch = c(15,1), col = c('black', 'grey'),main = "Salmonella Wet vs Dry Innoculation Methods")
dev.off()

jpeg('sal, wet~dry', units = 'in', width = 5, height =5, res =300)
low_awSal$Start = ifelse(low_awSal$Initial >=9,1,0)
xyplot(Rate~Temperature, groups =  Start, data = low_awSal,cex = 1, pch = c(15,1), col = c('black', 'grey'),main = "Salmonella Wet vs Dry Innoculation Methods")
dev.off()


####Listeria Images
low_awLis$STL = ifelse(low_awLis$Organism == '1/2a V7', 1, 
                      ifelse(low_awLis$Organism == '4b Scott A',2,
                             ifelse(low_awLis$Organism == '4b strain 302',3,0)))

jpeg('lis~strain', units ='in', width = 5, height = 5, res = 300)
xyplot(Rate ~ Temperature, groups = STL, data = low_awLis, cex =1, pch = c(1,1,1,1), col = c('black', 'royalblue2', ' forestgreen', 'sienna2'), main = "          Listeria Rate of Reduction vs 
          Temperature with Serovar", ylab = "Rate (logCFU/day/g)")
dev.off()

jpeg('lis~temp', units ='in', width = 5, height = 5, res = 300)
xyplot(Rate ~ Temperature, data = low_awLis, main = "         Listeria Rate of Reduction vs Temperature", ylab = "Rate (logCFU/day/g)",
       panel = function(x,y){
         panel.xyplot(x,y, col = 'black')
         panel.abline(lm(y~x),
                      col = 'red', lwd  =2)
       })


dev.off()

jpeg('lis~aw', units ='in', width = 5, height = 5, res = 300)
xyplot(Rate~Aw,data = low_awLis, main = "          Listeria Rate of Reduction 
          vs Water Activity", ylab = "Rate (logCFU/day/g)", xlab = "Water Activity",
       panel = function(x,y){
         panel.xyplot(x,y, col = 'black')
         panel.abline(lm(y~x),
                      col = 'red', lwd  =2)
       })
dev.off()



####Ecoli images
low_awEcoli$STE = ifelse(low_awEcoli$Organism == 'O157:H7 E0139', 1, 
                       ifelse(low_awEcoli$Organism == 'O157:H7 MB3885',2,
                              ifelse(low_awEcoli$Organism == 'O6 ATCC 25922',3,0)))

jpeg('ecoli~strain', units ='in', width = 5, height = 5, res = 300)
xyplot(Rate ~ Temperature, groups = STE, data = low_awEcoli, cex =1, pch = c(1,1,1,1), col = c('black', 'royalblue2', ' forestgreen', 'sienna2'),main = "          E. coli Rate of Reduction vs 
         Temperature with Serovar", ylab = "Rate (logCFU/day/g)")
dev.off()

jpeg('ecoli~temp', units ='in', width = 5, height = 5, res = 300)
xyplot(Rate ~ Temperature, data = low_awEcoli, main = "          E. coli Rate of Reduction vs Temperature", ylab = "Rate (logCFU/day/g)",
       panel = function(x,y){
         panel.xyplot(x,y, col = 'black')
         panel.abline(lm(y~x),
                      col = 'red', lwd  =2)
       })


dev.off()

jpeg('ecoli~aw', units ='in', width = 5, height = 5, res = 300)
xyplot(Rate~Aw, data=low_awEcoli, main = "          E. coli Rate of Reduction 
          vs Water Activity", ylab = "Rate (logCFU/day/g)", xlab = "Water Activity",
       panel = function(x,y){
         panel.xyplot(x,y, col = 'black')
         panel.abline(lm(y~x),
                      col = 'red', lwd  =2)
       })
dev.off()
