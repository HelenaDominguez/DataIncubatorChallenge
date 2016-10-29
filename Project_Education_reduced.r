##########################################################
#    Plots Education Project (Data Incubator Challenge)
###########################################################

rm(list=ls(all=TRUE))
setwd('/Users/Helena/Desktop/DataScience/Incubator/Education')

#Data source
#http://www2.ed.gov/about/inits/ed/edfacts/data-files/index.html

#Read files
#----------------------
filename="math-achievement-lea-sy2014-15.csv"
data <-read.csv(filename, header=TRUE)

filename="rla-achievement-lea-sy2014-15.csv"
data2 <-read.csv(filename, header=TRUE)

data[ , c(6:229)]<- sapply(data[ , c(6:229)], as.character)
data[ , c(6:229)]<- sapply(data[ , c(6:229)], as.numeric)

data2[ , c(6:229)]<- sapply(data2[ , c(6:229)], as.character)
data2[ , c(6:229)]<- sapply(data2[ , c(6:229)], as.numeric)

#Proficient students by Race
############################

par(mfrow=c(2,2))

hist(data$MWH_MTH00PCTPROF_1415, col=rgb(0,0,1,0.5), add=F, breaks=seq(0,100,5), 
     xlab="% of Proficient Students", ylab="Number of schools", main="White")
hist(data2$MWH_RLA00PCTPROF_1415, col=rgb(1,0,0,0.5), add=T, breaks=seq(0,100,5))

hist(data$MBL_MTH00PCTPROF_1415, col=rgb(0,0,1,0.5), add=F, breaks=seq(0,100,5), 
     xlab="% of Proficient Students", ylab="Number of schools", main="Black")
hist(data2$MBL_RLA00PCTPROF_1415, col=rgb(1,0,0,0.5), add=T, breaks=seq(0,100,5))
legend("topright", c('Math', 'RLA'),lty=c(1,1), lwd=c(2.5),col=c("blue", "red"))


hist(data$MHI_MTH00PCTPROF_1415, col=rgb(0,0,1,0.5), add=F, breaks=seq(0,100,5), 
     xlab="% of Proficient Students", ylab="Number of schools",main="Hispanic")
hist(data2$MHI_RLA00PCTPROF_1415, col=rgb(1,0,0,0.5), add=T, breaks=seq(0,100,5))

hist(data$MAS_MTH00PCTPROF_1415, col=rgb(0,0,1,0.5), add=F, breaks=seq(0,100,5), 
     xlab="% of Proficient Students",  ylab="Number of schools",main="Asian")
hist(data2$MAS_RLA00PCTPROF_1415, col=rgb(1,0,0,0.5), add=T, breaks=seq(0,100,5))

quartz.save(paste(getwd(),"/Prof_students_by_Race.png",sep=""),width=6.67,height=5.67)


# Disadvantage Box Plots
#####################


xx_RLA<-c(rep("All", length(data2$ALL_RLA00PCTPROF_1415)), rep("Eco. Disadv.", length(data2$ALL_RLA00PCTPROF_1415)),
          rep("Limit Eng.", length(data2$ALL_RLA00PCTPROF_1415)), rep("Migrant.", length(data2$ALL_RLA00PCTPROF_1415)),
          rep("Homeless", length(data2$ALL_RLA00PCTPROF_1415)))

xx_MTH<-c(rep("All", length(data$ALL_MTH00PCTPROF_1415)), rep("Eco. Disadv.", length(data$ALL_MTH00PCTPROF_1415)),
          rep("Limit Eng.", length(data$ALL_MTH00PCTPROF_1415)), rep("Migrant.", length(data$ALL_MTH00PCTPROF_1415)),
          rep("Homeless", length(data$ALL_MTH00PCTPROF_1415)))

yy_RLA<-c(data2$ALL_RLA00PCTPROF_1415, data2$ECD_RLA00PCTPROF_1415, data2$LEP_RLA00PCTPROF_1415,
          data2$MIG_RLA00PCTPROF_1415,data2$HOM_RLA00PCTPROF_1415)

yy_MTH<-c(data$ALL_MTH00PCTPROF_1415, data$ECD_MTH00PCTPROF_1415, data$LEP_MTH00PCTPROF_1415,
          data$MIG_MTH00PCTPROF_1415,data$HOM_MTH00PCTPROF_1415)

par(mfrow=c(2,1), oma = c(1, 3, 3, 3))
boxplot(yy_RLA~xx_RLA, main="RLA Porficiency", ylabel="Percentage of Prof. Students")

boxplot(yy_MTH~xx_MTH, main="Math Porficiency", ylabel="Percentage of Prof. Students")



quartz.save(paste(getwd(),"/Prof_students_Disadvantage.png",sep=""),width=7.67,height=7.67)


