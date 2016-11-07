

rm(list=ls(all=TRUE))
setwd('/Users/Helena/Desktop/DataScience/Incubator')

#Read files
#----------------------
filename="math-achievement-lea-sy2014-15.csv"
data <-read.csv(filename, header=TRUE)
data_original <-data

filename="rla-achievement-lea-sy2014-15.csv"
data2 <-read.csv(filename, header=TRUE)
data2_original <-data


data[ , c(6:229)]<- sapply(data[ , c(6:229)], as.character)
data[ , c(6:229)]<- sapply(data[ , c(6:229)], as.numeric)

data2[ , c(6:229)]<- sapply(data2[ , c(6:229)], as.character)
data2[ , c(6:229)]<- sapply(data2[ , c(6:229)], as.numeric)

#########Maths#################



#careful with nan
max(data$M_MTH00NUMVALID_1415 + data$F_MTH00NUMVALID_1415-data$ALL_MTH00NUMVALID_1415)




#Fraction male/female students
#######################################
par(mfrow=c(2,2))

hist(data$F_MTH00NUMVALID_1415/data$ALL_MTH00NUMVALID_1415*100, col=rgb(1,0,0,0.5), xlab="%", main="Math Valid 2014/2015")
hist(data$M_MTH00NUMVALID_1415/data$ALL_MTH00NUMVALID_1415*100, add=T, col=rgb(0,0,1,0.5))
legend("topright", c('Male', 'Female'),lty=c(1,1), lwd=c(2.5),col=c("blue", "red"))
box()

#RLA
hist(data2$F_RLA00NUMVALID_1415/data2$ALL_RLA00NUMVALID_1415*100, col=rgb(1,0,0,0.5), xlab="%", main="RLA Valid 2014/2015")
hist(data2$M_RLA00NUMVALID_1415/data2$ALL_RLA00NUMVALID_1415*100, add=T, col=rgb(0,0,1,0.5))
legend("topright", c('Male', 'Female'),lty=c(1,1), lwd=c(2.5),col=c("blue", "red"))
box()


#Math
hist(data$ALL_MTH00PCTPROF_1415, col='grey', xlab="%", main="Math Proficien 2014/2015")
hist(data$F_MTH00PCTPROF_1415, add=T, col=rgb(1,0,0,0.5))
hist(data$M_MTH00PCTPROF_1415, add=T, col=rgb(0,0,1,0.5))
legend("topright", c('Male', 'Female'),lty=c(1,1), lwd=c(2.5),col=c("blue", "red"))
box()

#RLA
hist(data2$ALL_RLA00PCTPROF_1415, col='grey', xlab="%", main="RLA Proficien 2014/2015")
hist(data2$F_RLA00PCTPROF_1415, add=T, col=rgb(1,0,0,0.5))
hist(data2$M_RLA00PCTPROF_1415, add=T, col=rgb(0,0,1,0.5))
legend("topright", c('Male', 'Female'),lty=c(1,1), lwd=c(2.5),col=c("blue", "red"))
box()


#Fraction races students
par(mfrow=c(2,2))
hist(data$MBL_MTH00NUMVALID_1415/data$ALL_MTH00NUMVALID_1415*100, col=rgb(1,0,0,0.5), xlab="%", main="Math Valid 2014/2015: Black")
legend("topright", c('Black', 'Asian', 'Hispanic', 'Native American'),lty=c(1,1,1,1), 
       lwd=c(2.5),col=c("red", "yellow", "green", "light blue"))
hist(data$MHI_MTH00NUMVALID_1415/data$ALL_MTH00NUMVALID_1415*100, add=F, col=rgb(0,1,0,0.5),  main="Hispanic") #green
hist(data$MAS_MTH00NUMVALID_1415/data$ALL_MTH00NUMVALID_1415*100, add=F, col=rgb(1,1,0,0.5),  main="Asian") # yellow




#Prof races students
par(mfrow=c(2,2))


hist(data$MWH_MTH00PCTPROF_1415, col=rgb(0,0,1,0.5), add=F, breaks=seq(0,100,5), xlab="%",  main="White")
hist(data2$MWH_RLA00PCTPROF_1415, col=rgb(1,0,0,0.5), add=T, breaks=seq(0,100,5))

#hist(data2$ALL_MTH00PCTPROF_1415, col="light grey", xlab="%",  breaks=seq(0,100,5), main="Black")
hist(data$MBL_MTH00PCTPROF_1415, col=rgb(0,0,1,0.5), add=F, breaks=seq(0,100,5), xlab="%", main="Black")
hist(data2$MBL_RLA00PCTPROF_1415, col=rgb(1,0,0,0.5), add=T, breaks=seq(0,100,5))
legend("topright", c('Math', 'RLA'),lty=c(1,1), lwd=c(2.5),col=c("blue", "red"))

#hist(data2$ALL_MTH00PCTPROF_1415, col="light grey", xlab="%", main="Hispanic" )
hist(data$MHI_MTH00PCTPROF_1415, col=rgb(0,0,1,0.5), add=F, breaks=seq(0,100,5), xlab="%", main="Hispanic")
hist(data2$MHI_RLA00PCTPROF_1415, col=rgb(1,0,0,0.5), add=T, breaks=seq(0,100,5))

#hist(data2$ALL_MTH00PCTPROF_1415, col="light grey", xlab="%",  main="Asian")
hist(data$MAS_MTH00PCTPROF_1415, col=rgb(0,0,1,0.5), add=F, breaks=seq(0,100,5), xlab="%",  main="Asian")
hist(data2$MAS_RLA00PCTPROF_1415, col=rgb(1,0,0,0.5), add=T, breaks=seq(0,100,5))



#PLOTS 
###########################

#Maths
#==========
par(mfrow=c(2,2))

plot(data$ALL_MTH00PCTPROF_1415, data$MWH_MTH00PCTPROF_1415, pch=3,xlab='% Math Prof. All', ylab='% Math Prof. White ')
abline(0, 1, col='red')

plot(data$ALL_MTH00PCTPROF_1415, data$MBL_MTH00PCTPROF_1415, pch=3, xlab='% Math Prof. All', ylab='% Math Prof. Black')
abline(0, 1, col='red')

plot(data$ALL_MTH00PCTPROF_1415, data$MHI_MTH00PCTPROF_1415, pch=3, xlab='% Math Prof. All', ylab='% Math Prof. Hispanic')
abline(0, 1, col='red')

plot(data$ALL_MTH00PCTPROF_1415, data$MAS_MTH00PCTPROF_1415, pch=3, xlab='% Math Prof. All', ylab='% Math Prof. Asian')
abline(0, 1, col='red')


#RLA
#==========
par(mfrow=c(2,2))

plot(data2$ALL_RLA00PCTPROF_1415, data2$MWH_RLA00PCTPROF_1415, pch=3,xlab='% Math Prof. All', ylab='% RLA Prof. White ')
abline(0, 1, col='red')

plot(data2$ALL_RLA00PCTPROF_1415, data2$MBL_RLA00PCTPROF_1415, pch=3, xlab='% Math Prof. All', ylab='% RLA Prof. Black')
abline(0, 1, col='red')

plot(data2$ALL_RLA00PCTPROF_1415, data2$MHI_RLA00PCTPROF_1415, pch=3, xlab='% Math Prof. All', ylab='% RLA Prof. Hispanic')
abline(0, 1, col='red')

plot(data2$ALL_RLA00PCTPROF_1415, data2$MAS_RLA00PCTPROF_1415, pch=3, xlab='% Math Prof. All', ylab='% RLA Prof. Asian')
abline(0, 1, col='red')



############## Disadvantage ###############
#Math
par(mfrow=c(2,2))

plot(data$ALL_MTH00PCTPROF_1415, data$ECD_MTH00PCTPROF_1415, pch=3,xlab='% Math Prof. All', ylab='% Math Prof. ECD ')
abline(0, 1, col='red')

plot(data$ALL_MTH00PCTPROF_1415, data$LEP_MTH00PCTPROF_1415, pch=3, xlab='% Math Prof. All', ylab='% Math Prof. LEP')
abline(0, 1, col='red')

plot(data$ALL_MTH00PCTPROF_1415, data$MIG_MTH00PCTPROF_1415, pch=3, xlab='% Math Prof. All', ylab='% Math Prof. MIG')
abline(0, 1, col='red')

plot(data$ALL_MTH00PCTPROF_1415, data$HOM_MTH00PCTPROF_1415, pch=3, xlab='% Math Prof. All', ylab='% Math Prof. HOM')
abline(0, 1, col='red')


#RLA
par(mfrow=c(2,2))

plot(data2$ALL_RLA00PCTPROF_1415, data2$ECD_RLA00PCTPROF_1415, pch=3,xlab='% RLA Prof. All', ylab='% RLA Prof. ECD ')
abline(0, 1, col='red')

plot(data2$ALL_RLA00PCTPROF_1415, data2$LEP_RLA00PCTPROF_1415, pch=3, xlab='%  RLA Prof. All', ylab='% RLA Prof. LEP')
abline(0, 1, col='red')

plot(data2$ALL_RLA00PCTPROF_1415, data2$MIG_RLA00PCTPROF_1415, pch=3, xlab='% RLA Prof. All', ylab='% RLA Prof. MIG')
abline(0, 1, col='red')

plot(data2$ALL_RLA00PCTPROF_1415, data2$HOM_RLA00PCTPROF_1415, pch=3, xlab='% RLA Prof. All', ylab='% RLA Prof. HOM')
abline(0, 1, col='red')

######## Box plots #############


kk_RLA<-c(rep("All", length(data2$ALL_RLA00PCTPROF_1415)), rep("Eco. Disadv.", length(data2$ALL_RLA00PCTPROF_1415)),
      rep("Limit Eng.", length(data2$ALL_RLA00PCTPROF_1415)), rep("Migrant.", length(data2$ALL_RLA00PCTPROF_1415)),
      rep("Homeless", length(data2$ALL_RLA00PCTPROF_1415)))

kk_MTH<-c(rep("All", length(data$ALL_MTH00PCTPROF_1415)), rep("Eco. Disadv.", length(data$ALL_MTH00PCTPROF_1415)),
      rep("Limit Eng.", length(data$ALL_MTH00PCTPROF_1415)), rep("Migrant.", length(data$ALL_MTH00PCTPROF_1415)),
      rep("Homeless", length(data$ALL_MTH00PCTPROF_1415)))

yy_RLA<-c(data2$ALL_RLA00PCTPROF_1415, data2$ECD_RLA00PCTPROF_1415, data2$LEP_RLA00PCTPROF_1415,
      data2$MIG_RLA00PCTPROF_1415,data2$HOM_RLA00PCTPROF_1415)

yy_MTH<-c(data$ALL_MTH00PCTPROF_1415, data$ECD_MTH00PCTPROF_1415, data$LEP_MTH00PCTPROF_1415,
      data$MIG_MTH00PCTPROF_1415,data$HOM_MTH00PCTPROF_1415)

par(mfrow=c(2,1))
boxplot(yy_RLA~kk_RLA, main="RLA Porficiency")
boxplot(yy_MTH~kk_MTH, main="Math Porficiency")




##### RLA vs MATH ###########

diff_RLA_MTH=data2$ALL_RLA00PCTPROF_1415-data$ALL_MTH00PCTPROF_1415

length(which(is.na(diff_RLA_MTH) == FALSE)) #9276
rare=which(abs(diff_RLA_MTH) > 3*sd(diff_RLA_MTH, na.rm = TRUE)) #136 , 1.5%


par(mfrow=c(1,2))

hist(diff_RLA_MTH, right=FALSE, freq=FALSE)
curve(dnorm(x, mean=mean(diff_RLA_MTH, na.rm=TRUE), sd=sd(diff_RLA_MTH, na.rm=TRUE)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

legend("topleft", c(paste('Mean=',round(mean(diff_RLA_MTH, na.rm=TRUE),digits=2 )),
                    paste("Stdv=", round(sd(diff_RLA_MTH, na.rm=TRUE),digits=2))), box.lty=0)


plot(data2$ALL_RLA00PCTPROF_1415, data$ALL_MTH00PCTPROF_1415,
     pch=4, xlab='% RLA Prof. All', ylab='% MTH Prof. ALL')

points(data2$ALL_RLA00PCTPROF_1415[rare], data$ALL_MTH00PCTPROF_1415[rare],
     pch=4, col='blue')
abline(0, 1, col='red')





##### Best schools by State ###########

#Math
#--------------

#install.packages("plotrix")
#library(plotrix)

par(mfrow=c(2,1))

PROF_MTH=which(data$ALL_MTH00PCTPROF_1415 > 80) #745
length(PROF_MTH)/length(data$ALL_MTH00PCTPROF_1415) #sim5 % !! whats out NAN

state<-unique(data$STNAM[PROF_MTH])
pc=c(1:length(state))
num_class=c(1:length(state)) 

for (i in 1:length(state)){
        num_class[i]=length(which(data$STNAM[PROF_MTH]==state[i]))
        pc[i]=length(which(data$STNAM[PROF_MTH]==state[i]))/length(PROF_MTH)*100 
        
}

pc_sig=pc[which(pc > 1.7)]
state_sig=state[which(pc > 1.7)]

pie3D(pc_sig[order(pc_sig)],labels=state_sig[order(pc_sig)],explode=0.1,
      main="Prof. RLA by states", labelcex=0.7, col=rainbow(length(pc_sig)))  

#RLA
#--------------

PROF_RLA=which(data2$ALL_RLA00PCTPROF_1415 > 80) #1027
length(PROF_RLA)/length(data2$ALL_RLA00PCTPROF_1415) #sim 6 % !! whats out NAN

state<-unique(data2$STNAM[PROF_RLA])
pc=c(1:length(state))
num_class=c(1:length(state)) 

for (i in 1:length(state)){
        num_class[i]=length(which(data2$STNAM[PROF_RLA]==state[i]))
        pc[i]=length(which(data2$STNAM[PROF_RLA]==state[i]))/length(PROF_RLA)*100 
        
}

pc_sig=pc[which(pc > 1.7)]
state_sig=state[which(pc > 1.7)]

pc_sig=c(pc_sig,100-sum(pc_sig))
state_sig=c(as.character(state_sig),"Others")

pie3D(pc_sig[order(pc_sig)],labels=state_sig[order(pc_sig)],explode=0.1,
      main="Prof. RLA by states", labelcex=0.7, col=rainbow(length(pc_sig)))  

