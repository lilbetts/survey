library(RCurl)
out <- postForm("http://172.16.12.3/redcap/api/",
                token="0D0DC5F8F3563DEF764043A1631250F4",
                content="record",
                type="flat",
                format="csv",
                .opts=curlOptions(ssl.verifypeer=FALSE))
write(out,file="H:/kwtrp/out.csv")
setwd("H:/kwtrp")
survey<-read.csv("out.csv", header=T)
head(survey)
##variable names
names(survey)
##rename variables
names(survey)<-c("id","role","orole","edulvl","proflvl","muse",
                 "mexper","lpref","on1","on2","on3","on4",
                 "on5","on6","f2f1","f2f2","f2f3",  
                 "f2f4","stata","R","spss","SAS","osft",
                 "none","osft1","psft","opsft","tp1",                
                 "tp2","tp3","tp4","tp5","tp6","tp7","tp8","tp9",                
                 "tp10","otp","idea","complete")
##new variable names
names(survey)


##table1<- distribution of the proficiency levels of statistical analysis
##table2<-association between mooc use and prefered mode of learning
##table3<-mooc use and experience with them
##table4<-project rating

##attach dataframe to search path
attach(survey) 
## create a new variable named "group" to split "role" into interns and non interns.
survey$group[role==1] <- "interns" 
survey$group[role!=1] <- "noninterns"


library(ggplot2)## graphics package

## table1

tb1.1<-table(survey$proflvl,survey$group)
p1.1<-ggplot(survey[!is.na(survey$group),],aes(proflvl, fill=group)) + geom_bar(position="dodge")
p1.1+ scale_x_discrete(breaks=1:4,labels=c("veryproficient","proficient","moderate","novice" )) + ggtitle("DISTRIBUTION OF  LEVELS OF STATISTICAL ANALYSIS") + xlab("Proficiency levels")
##create a new variable"edu"
survey$edu[survey$edulvl==1]<-"bachelors"
survey$edu[survey$edulvl==2]<-"masters"
survey$edu[survey$edulvl==3]<-"phd"
survey$edu[survey$edulvl==4]<-"bachelors"

tb1.2<-table(survey$proflvl,survey$edu)
p1.2<-ggplot(survey[!is.na(survey$edu),],aes(proflvl, fill=edu)) + geom_bar(position="dodge")
p1.2+ scale_x_discrete(breaks=1:4,labels=c("veryproficient","proficient","moderate","novice" ))+ggtitle("DISTRIBUTION OF LEVELS OF STATISTICAL ANALYSIS") + xlab("Proficiency levels")


##table2
survey$muse1[survey$muse==1]<-"yes"
survey$muse1[survey$muse==2]<-"no"

survey$lpref1[survey$lpref==1]<-"online"
survey$lpref1[survey$lpref==2]<-"face to face"
survey$lpref1[survey$lpref==3]<-"no preference"
tb2<-table(survey$muse1,survey$lpref1)

fisher.test(tb2)

##table3

survey$m.1[survey$muse==1]<-"yes"##create new variable
survey$experience[survey$mexper==1]<-"veryuseful"
survey$experience[survey$mexper==2]<-"useful"
survey$experience[survey$mexper==3]<-"not useful"
tb3<-table(survey$m.1,survey$experience)
tb3

##table4
tb4<-table(idea,survey$group)
p4.1<-ggplot(survey[!is.na(survey$group),],aes(idea,fill=group))+ geom_bar(position="dodge",binwidth=0.5)
p4.1+ scale_x_discrete(breaks=1:3,labels=c("veryuseful","useful","not useful")) + ggtitle("PROJECT RATING") + xlab("project")
detach(survey)

attach(survey)
##create another variable  "stats" to breakdown "role" into statisticians,non-statisticians and interns
survey$stats[role==7]<-"statisticians"
survey$stats[role!=7]<-"nonstatisticians"
survey$stats[role==1]<-"interns"

p4.2<-ggplot(survey[!is.na(survey$stats),],aes(idea,fill=stats))+ geom_bar(position="dodge",binwidth=0.5)
p4.2 + scale_x_discrete(breaks=1:3,labels=c("veryuseful","useful","notuseful")) + ggtitle("PROJECT RATING") + xlab("project")

detach(survey)

p4.2<-ggplot(survey[!is.na(survey$stats),],aes(idea,fill=stats))+ geom_bar(position="dodge",binwidth=0.5)
p4.2  + ggtitle("PROJECT RATING") + xlab("project")



        