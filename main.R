# Created by: Chris Molina
# Created on: 9/3/2020
library(dplyr)
#Data Wrangling
Students1 <- read.csv("student-mat.csv")
Students2 <- read.csv("student-por.csv")
Students <- merge(Students1,Students2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
Student <- select(Students, studytime.x,failures.x, failures.y, freetime.x, goout.x, Dalc.x, Walc.x,absences.x, absences.y,G3.x,G3.y)

#Combining seperated variables
Student$failuresTotal <- Student$failures.y + Student$failures.x
Student$absencesTotal <- Student$absences.y + Student$absences.x
Student$alcTotal <- Student$Walc.x + Student$Dalc.x
Student$G3Total <- Student$G3.x + Student$G3.y
Student<- select(Student, studytime.x,failuresTotal, Dalc.x, Walc.x, alcTotal, absencesTotal, G3.x, G3.y, G3Total)


#Seperating each alc total into its own dataset
alc1 <- subset(Student, alcTotal == 1)
alc2 <- subset(Student, alcTotal == 2)
alc3 <- subset(Student, alcTotal == 3)
alc4 <- subset(Student, alcTotal == 4)
alc5 <- subset(Student, alcTotal == 5)
alc6 <- subset(Student, alcTotal == 6)
alc7 <- subset(Student, alcTotal == 7)
alc8 <- subset(Student, alcTotal == 8)
alc9 <- subset(Student, alcTotal == 9)
alc10 <- subset(Student, alcTotal == 10)

#Bargraph names.arg
NamesArg <- c("1","2","3","4","5","6","7","8","9","10")

#alc vs Absences
#Sums of absences
alc1Absences <- sum(alc1$absencesTotal)/nrow(alc1)
alc2Absences <- sum(alc2$absencesTotal)/nrow(alc2)
alc3Absences <- sum(alc3$absencesTotal)/nrow(alc3)
alc4Absences <- sum(alc4$absencesTotal)/nrow(alc4)
alc5Absences <- sum(alc5$absencesTotal)/nrow(alc5)
alc6Absences <- sum(alc6$absencesTotal)/nrow(alc6)
alc7Absences <- sum(alc7$absencesTotal)/nrow(alc7)
alc8Absences <- sum(alc8$absencesTotal)/nrow(alc8)
alc9Absences <- sum(alc9$absencesTotal)/nrow(alc9)
alc10Absences <- sum(alc10$absencesTotal)/nrow(alc10)
Absences <- c(alc1Absences,alc2Absences,alc3Absences,alc4Absences,alc5Absences,alc6Absences,alc7Absences,alc8Absences,alc9Absences,alc10Absences)
barplot(Absences, names.arg = NamesArg, xlab = "Total Alcohol Consumption", ylab = "Average Absences")

AbsencesColumn <- select(Student, absencesTotal)
AbsencesMean <- sum(AbsencesColumn)/nrow(AbsencesColumn)
#SD
XMinusMean <- 0
for(i in 1:nrow(AbsencesColumn)){
  X <- (AbsencesColumn[i,1] - AbsencesMean)**2
  XMinusMean <- XMinusMean + X
}
AbsencesSD <- sqrt(XMinusMean/nrow(AbsencesColumn))


#alc vs Total Grades
#Sums of Grades
alc1G3 <- sum(alc1$G3Total)/nrow(alc1)
alc2G3 <- sum(alc2$G3Total)/nrow(alc2)
alc3G3 <- sum(alc3$G3Total)/nrow(alc3)
alc4G3 <- sum(alc4$G3Total)/nrow(alc4)
alc5G3 <- sum(alc5$G3Total)/nrow(alc5)
alc6G3 <- sum(alc6$G3Total)/nrow(alc6)
alc7G3 <- sum(alc7$G3Total)/nrow(alc7)
alc8G3 <- sum(alc8$G3Total)/nrow(alc8)
alc9G3 <- sum(alc9$G3Total)/nrow(alc9)
alc10G3 <- sum(alc10$G3Total)/nrow(alc10)
Grades <- c(alc1G3,alc2G3,alc3G3,alc4G3,alc5G3,alc6G3,alc7G3,alc8G3,alc9G3,alc10G3)
barplot(Grades, names.arg =  NamesArg, xlab = "Total Alcohol Consumption", ylab = "Average Grades")

#Finding Grade averages and Standard Deviation
GradeColumn <- select(Student, G3Total)
GradeMean <- sum(GradeColumn)/nrow(GradeColumn)
#SD
XMinusMean <- 0
for(i in 1:nrow(GradeColumn)){
  X <- (GradeColumn[i,1] - GradeMean)**2
  XMinusMean <- XMinusMean + X
}
GradeSD <- sqrt(XMinusMean/nrow(GradeColumn))

#Failures
alc1Fail <- sum(alc1$failuresTotal)/nrow(alc1)
alc2Fail <- sum(alc2$failuresTotal)/nrow(alc2)
alc3Fail <- sum(alc3$failuresTotal)/nrow(alc3)
alc4Fail <- sum(alc4$failuresTotal)/nrow(alc4)
alc5Fail <- sum(alc5$failuresTotal)/nrow(alc5)
alc6Fail <- sum(alc6$failuresTotal)/nrow(alc6)
alc7Fail <- sum(alc7$failuresTotal)/nrow(alc7)
alc8Fail <- sum(alc8$failuresTotal)/nrow(alc8)
alc9Fail <- sum(alc9$failuresTotal)/nrow(alc9)
alc10Fail <- sum(alc10$failuresTotal)/nrow(alc10)
Fails <- c(alc1Fail, alc2Fail, alc3Fail, alc4Fail, alc5Fail, alc6Fail, alc7Fail, alc8Fail, alc9Fail, alc10Fail)
barplot(Fails, names.arg =  NamesArg, xlab = "Total Alcohol Consumption", ylab = "Average Failed Classes")

#Finding Fail averages and Standard Deviation
FailColumn <- select(Student, failuresTotal)
FailMean <- sum(FailColumn)/nrow(FailColumn)
#SD
XMinusMean <- 0
for(i in 1:nrow(FailColumn)){
  X <- (FailColumn[i,1] - FailMean)**2
  XMinusMean <- XMinusMean + X
}
FailSD <- sqrt(XMinusMean/nrow(FailColumn))

#Study Time
alc1study <- sum(alc1$studytime.x)/nrow(alc1)
alc2study <- sum(alc2$studytime.x)/nrow(alc2)
alc3study <- sum(alc3$studytime.x)/nrow(alc3)
alc4study <- sum(alc4$studytime.x)/nrow(alc4)
alc5study <- sum(alc5$studytime.x)/nrow(alc5)
alc6study <- sum(alc6$studytime.x)/nrow(alc6)
alc7study <- sum(alc7$studytime.x)/nrow(alc7)
alc8study <- sum(alc8$studytime.x)/nrow(alc8)
alc9study <- sum(alc9$studytime.x)/nrow(alc9)
alc10study <- sum(alc10$studytime.x)/nrow(alc10)
study <- c(alc1study, alc2study,alc3study,alc4study,alc5study,alc6study,alc7study,alc8study,alc9study,alc10study)
barplot(study, names.arg =  NamesArg, xlab = "Total Alcohol Consumption", ylab = "Average study time")

#Finding studytime averages and Standard Deviation
StudyColumn <- select(Student, studytime.x)
StudyMean <- sum(StudyColumn)/nrow(StudyColumn)
#SD
XMinusMean <- 0
for(i in 1:nrow(StudyColumn)){
  X <- (StudyColumn[i,1] - StudyMean)**2
  XMinusMean <- XMinusMean + X
}
StudySD <- sqrt(XMinusMean/nrow(StudyColumn))

#printing averages and standard deviations
print("absent mean")
print(AbsencesMean)
print("absentSD")
print(AbsencesSD)
print("grades mean")
print(GradeMean)
print("grade SD")
print(GradeSD)
print("Fail mean")
print(FailMean)
print("Fail SD")
print(FailSD)
print("Studytime mean")
print(StudyMean)
print("StudytimeSD")
print(StudySD)







