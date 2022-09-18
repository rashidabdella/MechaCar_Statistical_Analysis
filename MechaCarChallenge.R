library(dplyr)

MechaCar_mpg <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar_mpg)

summary(lm(mpg ~ vehicle_weight + vehicle_length + spoiler_angle + ground_clearance + AWD, data = MechaCar_mpg))



Suspension_Coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

summarize_Suspension_Coil <- Suspension_Coil %>% summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), Variance_PSI=var(PSI), SD=sd(PSI), .groups = 'keep') #create summary table


lot_summary_Suspension_COil <- Suspension_Coil%>% group_by(Manufacturing_Lot) %>%
summarize(Mean_PSI=mean(PSI), Median_PSI=median(PSI), Variance_PSI=var(PSI), SD=sd(PSI), .groups = 'keep')

t.test(Suspension_Coil$PSI,mu = 1500)

t.test(subset(Suspension_Coil, Manufacturing_Lot=="Lot1")$PSI,mu = 1500)

t.test(subset(Suspension_Coil, Manufacturing_Lot=="Lot2")$PSI,mu = 1500)

t.test(subset(Suspension_Coil, Manufacturing_Lot=="Lot3")$PSI,mu = 1500)

