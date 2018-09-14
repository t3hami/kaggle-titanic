titanic.train <- read.csv("train.csv", header = TRUE)
titanic.test <- read.csv("test.csv", header = TRUE)
rownames(titanic.train) <- titanic.train$PassengerId
rownames(titanic.test) <- titanic.test$passengerId

test_survived <- data.frame(Survived = rep("None", nrow(titanic.test)), titanic.test[,])
# Combine data sets
titanic.combined <- rbind(titanic.train, test_survived)

titanic.combined$Survived <- as.factor(titanic.combined$Survived)
titanic.combined$Pclass <- as.factor(titanic.combined$Pclass)

extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  }   else {
    return ("Other")
  }
}

titles <- NULL
for (i in 1:nrow(titanic.combined)) {
  titles <- c(titles, extractTitle(titanic.combined[i,"Name"]))
}
titanic.combined$title <- as.factor(titles)
titanic.train$title <- as.factor(titles)[1:891]
contingency_table <- table(titanic.train$title,titanic.train$Pclass, titanic.train$Survived)

predictions <- data.frame(PassengerId = titanic.test$PassengerId)
predictions$Survived <- 0
# Setting values by prop.table(contingency_table) * 418
mr1_survive = 18
other1_survive = 4
master3_survive = 5
miss3_survive = 24
mrs3_survive = 10
Survived <- NULL
for (i in 1:nrow(titanic.test)) {
  passenger_class <- titanic.test[i, "Pclass"]
  passenger_title <- extractTitle(titanic.test[i, "Name"])
  if (passenger_class == 1){
    if (
      passenger_title == "Miss." ||
      passenger_title == "Master." ||
      passenger_title == "Mrs."
      ) {
      Survived <- c(Survived, 1)
    }
    else if (passenger_title == "Mr.") {
      if (mr1_survive > 0) {
        Survived <- c(Survived, 1)
        mr1_survive = mr1_survive - 1
      }
      else {
        Survived <- c(Survived, 0)
      }
    }
    else if (passenger_title == "Other") {
      if (other1_survive > 0) {
        Survived <- c(Survived, 1)
        other1_survive = other1_survive - 1
      }
      else {
        Survived <- c(Survived, 0)
      }      
    }
  }
  else if (passenger_class == 2) {
    if (
      passenger_title == "Miss." ||
      passenger_title == "Master." ||
      passenger_title == "Mrs."
    ) {
      Survived <- c(Survived, 1)
    }
    if (passenger_title == "Other" || passenger_title == "Mr.") {
      Survived <- c(Survived, 0)
    }
    
  }
  else if (passenger_class == 3) {
    if (passenger_title == "Other" || passenger_title == "Mr.") {
      Survived <- c(Survived, 0)
    }
    else if (passenger_title == "Master.") {
      if (mr1_survive > 0) {
        Survived <- c(Survived, 1)
        master3_survive = master3_survive - 1
      }
      else {
        Survived <- c(Survived, 0)
      }
    }
    else if (passenger_title == "Miss.") {
      if (mr1_survive > 0) {
        Survived <- c(Survived, 1)
        miss3_survive = miss3_survive - 1
      }
      else {
        Survived <- c(Survived, 0)
      }
    }
    else if (passenger_title == "Mrs.") {
      if (mr1_survive > 0) {
        Survived <- c(Survived, 1)
        mrs3_survive = mrs3_survive - 1
      }
      else {
        Survived <- c(Survived, 0)
      }
    }
  }
}
predictions$Survived <- Survived
write.csv(predictions, file = "phase2_predictions.csv", row.names=FALSE)
#test <- titanic.train[titanic.train$Sex == "female", ]