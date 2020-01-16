library(dplyr)
library(nortest)
library(ggplot2)

#import data
vehicles<-read.csv("C:\\Users\\om96\\OneDrive\\Documents\\UTD\\Stats for Data Science - 6359\\fullspecs_data_cleaned.csv")

#explore data
summary(vehicles)

#see MSRP distribution
hist(vehicles$MSRP, main = "Histogram of MSRP", xlab = "MSRP")
boxplot(vehicles$MSRP, main = "Boxplot of MSRP",ylab = "MSRP")

#since it is so heavily skewed, take the log of the MSRP
vehicles$lMSRP<-log(vehicles$MSRP)

#now view the log MSRP distributions
hist(vehicles$lMSRP, main = "Histogram of Log(MSRP)", xlab = "Log(MSRP)")
boxplot(vehicles$lMSRP, main = "Boxplot of Log(MSRP)",ylab = "Log(MSRP)")

hist(log(vehicles$MSRP,10000), main = "Histogram of Log(MSRP)", xlab = "Log(MSRP)")
boxplot(log10(vehicles$lMSRP), main = "Boxplot of Log(MSRP)",ylab = "Log(MSRP)")



#change Year to represent years before 2019
vehicles$Year<-2019-vehicles$Year

#see how MSRP changes depending on the year of the model
plot(vehicles$Year,vehicles$lMSRP,main="Car's Age in 2019 vs Log(MSRP)",xlab = "(Year of Production)-2019",ylab="Log(MSRP)")

vehicles$Drivetrain<-factor(vehicles$Drivetrain,levels = c("FWD","4WD","RWD","AWD"))
vehicles$Fuel_Type<-factor(vehicles$Fuel_Type,levels = c("Gas","Diesel","Electric"))

#see how many different cars there are per manufacturer and the stats of the MSRP dependent on the manufacturer
manufacturer_summary<-vehicles %>%
  group_by(Manufacturer) %>%
  summarise("# of cars"=n(),"Average MSRP"=mean(lMSRP),"Median MSRP"=median(lMSRP),"Min MSRP" = min(lMSRP),"Max MSRP" = max(lMSRP))
manufacturer_summary

plot(manufacturer_summary$`# of cars`,manufacturer_summary$'Median MSRP', main = "# of Cars per Manufacturer vs Median Log(MSRP)",
       xlab = "# of cars", ylab = "Median Log(MSRP)",cex=0.1 )
text(manufacturer_summary$`# of cars`,manufacturer_summary$'Median MSRP',labels = manufacturer_summary$Manufacturer,cex=0.7)



round(cor(NewVehicles[,c(10:11,13:16)],use = "pairwise.complete.obs"),2)


cor_airbags<-NewVehicles[,43:49]
cor_airbags<-ifelse(cor_airbags=="Yes",1,0)

round(cor(cor_airbags,use = "pairwise.complete.obs"),2)

round(cor(NewVehicles$Net_Torque,NewVehicles$Net_HorsePower,use = "pairwise.complete.obs"),2)






NewVehicles<-vehicles[vehicles$Year<=5,]

condition <- NewVehicles$Manufacturer %in% c("Lamborghini" , "Rolls Royce" , "McLaren" , "Ferrari", "Bentley","Aston Martin")

NewVehicles$Luxury<- condition

boxplot(NewVehicles$Luxury,NewVehicles$lMSRP)

plot(log(NewVehicles$Net_HP_Needed),NewVehicles$lMSRP)

plot(log(NewVehicles$Net_HP.RPM),NewVehicles$lMSRP)

plot(log(NewVehicles$Fuel_Economy_Est.Combined_.MPG.),NewVehicles$lMSRP)
  
plot(log(NewVehicles$Net_HorsePower),NewVehicles$lMSRP)

plot(log(NewVehicles$Net_Torque),NewVehicles$lMSRP)

plot((NewVehicles$Fuel_Tank_Capacity._Approx_.gal.),NewVehicles$lMSRP)
ggplot(NewVehicles,aes(Body_Style,Fuel_Tank_Capacity._Approx_.gal.))+geom_boxplot()

plot((NewVehicles$Turning_Diameter_._Curb_to_Curb_.ft.),NewVehicles$lMSRP)
ggplot(NewVehicles,aes(Body_Style,Turning_Diameter_._Curb_to_Curb_.ft.))+geom_boxplot()

smallest <- lMSRP ~ 1
biggest <- lMSRP ~ Year + Engine_Type + Engine_Configuration + Cylinders+ Drivetrain + Passenger_Capacity + Passenger_Doors + 
  Body_Style + Transmission_Type +Second_Shoulder_Room_.in. +
  Fuel_Tank_Capacity._Approx_.gal. + log(Fuel_Economy_Est.Combined_.MPG.)+
  log(Net_Torque)+
  Fuel_Type + log(Net_HorsePower)+log(Net_HP.RPM)+Net_HP.RPM+log(Net_HP_Needed)+
  Steering_Type+
  Turning_Diameter_._Curb_to_Curb_.ft.+ 
  Air_Bag.Side_Body.Front + Air_Bag.Side_Body.Rear  + 
  Air_Bag.Side_Head.Rear + Child_Safety_Rear_Door_Locks + 
  Daytime_Running_Lights + Traction_Control + Night_Vision + 
  Rollover_Protection_Bars + Fog_Lamps + Parking_Aid +  
  Back.Up_Camera + Stability_Control  + Suspension_Type
m <- lm(lMSRP ~ 1, data=na.exclude(NewVehicles))
stats::step(m, scope=list(lower=smallest, upper=biggest)) # it does forward selection of the variables for us

optimized<-lm(formula = lMSRP ~ Suspension_Type + Engine_Type + Transmission_Type + 
                Parking_Aid + Daytime_Running_Lights + (Net_HorsePower) + 
                Air_Bag.Side_Head.Rear + Fuel_Tank_Capacity._Approx_.gal. + 
                Drivetrain + Fuel_Type + Turning_Diameter_._Curb_to_Curb_.ft. + 
                Passenger_Capacity + Body_Style + Child_Safety_Rear_Door_Locks + 
                Back.Up_Camera + Steering_Type + Rollover_Protection_Bars + 
                log(Net_HP_Needed) + Second_Shoulder_Room_.in. + Stability_Control + 
                (Fuel_Economy_Est.Combined_.MPG.), data = na.exclude(NewVehicles))
summary(optimized)

NewVehicles$Convertible<-if_else(NewVehicles$Body_Style=="Convertible",1,0)
NewVehicles$Steering_Type<-factor(NewVehicles$Steering_Type,levels=c("Rack_Pinion_steering","Recirculating_Ball_Steering","Electric_steering","Variable_steering"))

finallm<-lm(formula = lMSRP ~ Suspension_Type + Engine_Type + Transmission_Type + Drivetrain +Passenger_Capacity + Convertible +
              Fuel_Type +
              Fuel_Tank_Capacity._Approx_.gal.+(Fuel_Economy_Est.Combined_.MPG.) + (Net_HorsePower)  + Steering_Type +
              Child_Safety_Rear_Door_Locks + Parking_Aid +
              Back.Up_Camera
              , data = (NewVehicles))

summary(finallm)
plot(finallm)


coefs_exp<-data.frame("Exp(Coefficients)"=coef(finallm)[c(1,2,6,8:10,12,16:22,24:33,35,37:39)] )
coefs_exp<-round(exp(coefs_exp),3)
coefs_exp[1,1]
coefs_exp

finallm_manu<-lm(formula = lMSRP ~ Suspension_Type + Engine_Type + Transmission_Type + 
                   Parking_Aid + Daytime_Running_Lights + (Net_HorsePower) + 
                   Air_Bag.Side_Head.Rear + Fuel_Tank_Capacity._Approx_.gal. + 
                   Drivetrain + Fuel_Type + Turning_Diameter_._Curb_to_Curb_.ft. + 
                   Passenger_Capacity + Body_Style + Child_Safety_Rear_Door_Locks + 
                   Back.Up_Camera + Steering_Type + Rollover_Protection_Bars + 
                   log(Net_HP_Needed) + Second_Shoulder_Room_.in. + Stability_Control + 
                   (Fuel_Economy_Est.Combined_.MPG.)+Manufacturer, data = (NewVehicles))


summary(finallm_manu)
plot(finallm)


coefs_exp_manu<-data.frame("Exp(Coefficients)"=coef(finallm_manu)[c(1,2,6,8:10,12,16:22,24:33,35,37:80)] )
coefs_exp_manu<-round(exp(coefs_exp_manu),3)
coefs_exp_manu[1,1]
coefs_exp_manu


finallmi<-lm(formula = lMSRP ~ (Suspension_Type + Engine_Type + Transmission_Type + Drivetrain +Passenger_Capacity + Convertible +
              Fuel_Type +
              Fuel_Tank_Capacity._Approx_.gal.+(Fuel_Economy_Est.Combined_.MPG.) + (Net_HorsePower)  + Steering_Type +
              Child_Safety_Rear_Door_Locks + Parking_Aid +
              Back.Up_Camera)^2
            , data = (NewVehicles))

summary(finallmi)
plot(finallmi)

woElectric<-NewVehicles[NewVehicles$Engine_Type!="Electric",]
woElectric$predicted_val<-predict(finallm,woElectric)


woElectric$predicted_val<-exp(woElectric$predicted_val)

woElectric$Difference<-(woElectric$MSRP-woElectric$predicted_val)/woElectric$predicted_val+1


summary(woElectric$Difference)

plot(woElectric$MSRP,woElectric$predicted_val)

car<-woElectric[!is.na(woElectric$Difference),c(68,3,4,11,72,73)]

avgcar<- car %>%
  group_by(Model_Type, Manufacturer) %>%
  summarize(MSRP = mean(MSRP),Predicted_Value = mean(predicted_val),Percent_Difference = mean(Difference))
  

plot(avgcar$MSRP,avgcar$Predicted_Value)

car$Car<-as.character(car$Car)
summary(car$Difference)

summary(avgcar$Percent_Difference)

boxplot(avgcar$Percent_Difference)

head(avgcar[order(avgcar$Percent_Difference,decreasing = T),],n=10)

head(avgcar[order(avgcar$Percent_Difference,decreasing = F),],n=10)

normal_car<-avgcar[avgcar$MSRP<=50000,]
head(normal_car[order(normal_car$Percent_Difference,decreasing = F),],n=10)

worst_value<-car %>%
  group_by(Manufacturer) %>%
  summarise(Name = Car[which.max(Difference)], MSRP = MSRP[which.max(Difference)],predicted_val = predicted_val[which.max(Difference)],Difference = max(Difference))

best_value<-car %>%
  group_by(Manufacturer) %>%
  summarise(Name = Car[which.min(Difference)], MSRP = MSRP[which.min(Difference)],predicted_val = predicted_val[which.min(Difference)],Difference = min(Difference))

body_style_value<-car %>%
  group_by(Body_Style) %>%
  summarise(Name = Model_Type[which.min(Difference)], MSRP = MSRP[which.min(Difference)],predicted_val = predicted_val[which.min(Difference)],Difference = min(Difference))

manucar<-avgcar %>%
  group_by(Manufacturer) %>%
  summarise(n=n(),medval = median(Percent_Difference))

body<- car %>%
  group_by(Model_Type, Manufacturer) %>%
  summarize(MSRP = mean(MSRP),Predicted_Value = mean(predicted_val),Percent_Difference = mean(Difference))

body_style_value<-car %>%
  group_by(Body_Style) %>%
  summarise(MSRP = median(MSRP),predicted_val = median(predicted_val),Difference = median(Difference))

  