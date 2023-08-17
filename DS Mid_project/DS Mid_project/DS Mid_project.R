dataset<-read.csv("Dataset_midterm_Section(B).csv")
print(dataset)

is.na(dataset)
sum(is.na(dataset))

dataset$Gender[is.na(dataset$Gender)]<-mean(dataset$Gender,na.rm=TRUE)
dataset$age[is.na(dataset$age)]<-mean(dataset$age,na.rm=TRUE)
dataset$sibsp[is.na(dataset$sibsp)]<-mean(dataset$sibsp,na.rm=TRUE)
dataset$parch[is.na(dataset$parch)]<-mean(dataset$parch,na.rm=TRUE)
dataset$fare[is.na(dataset$fare)]<-mean(dataset$fare,na.rm=TRUE)
dataset$alone[is.na(dataset$alone)]<-mean(dataset$alone,na.rm=TRUE)
dataset$survived[is.na(dataset$survived)]<-mean(dataset$survived,na.rm=TRUE)

print(dataset)

dataset$Gender <- as.integer(dataset$Gender)
dataset$age <- as.integer(dataset$age)
dataset$fare <- as.integer(dataset$fare)

print(dataset)

dataset$class <- ifelse(dataset$class == "Third",3,
                        ifelse(dataset$class == "Second",2,
                               ifelse(dataset$class == "First",1,NA)))

dataset$class[is.na(dataset$class)]<-mean(dataset$class,na.rm=TRUE)
dataset$class <- as.integer(dataset$cla
                            ss)
print(dataset)

dataset$class <- ifelse(dataset$class == 3,"Third",
                        ifelse(dataset$class == 2,"Second",
                               ifelse(dataset$class == 1,"First",NA)))
print(dataset)

dataset$alone <- ifelse(dataset$alone == 1,"TRUE",
                        ifelse(dataset$alone == 0,"FALSE",NA))

print(dataset)
\
dataset$who <- ifelse(dataset$who == "womannn","WOMAN",
                      ifelse(dataset$who == "womann","WOMAN",
                             ifelse(dataset$who == "woman","WOMAN",
                                    ifelse(dataset$who == "mannn","MAN",
                                           ifelse(dataset$who == "man","MAN",
                                                  ifelse(dataset$who == "child","CHILD",NA))))))

print(dataset)
sum(is.na(dataset))