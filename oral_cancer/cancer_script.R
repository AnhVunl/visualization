# Loading data and packages

library(dplyr)
library(tidyverse);
library(ggplot2);
library(epitools);
library(tibble);
library(utils)
library(lubridate)
library(survival)
library(survminer)
library(caret)

# Some data cleaning and pre-processing
IKNL.data <-read.csv("/IKNL_data_oral_cavity.csv",  sep=';', header=TRUE) %>%  
  as.data.frame()
IKNL.data$gender <- factor(IKNL.data$gesl, labels = c("male", "female")) 
IKNL.data$diffgr <- factor(IKNL.data$diffgr)
IKNL.data$vital_status <- !is.na(IKNL.data$ovldat)
IKNL.data$vital_status[IKNL.data$vital_status== FALSE] <- 0 
IKNL.data$vital_status[IKNL.data$vital_status== TRUE] <- 1 
IKNL.data$vital_status <- factor(IKNL.data$vital_status, levels = c(0,1), 
  labels = c("Alive", "Deceased"))
IKNL.data$vital_status.binary <- !is.na(IKNL.data$ovldat)
IKNL.data$vital_status.binary[IKNL.data$vital_status.binary == FALSE] <- 0
IKNL.data$vital_status.binary[IKNL.data$vital_status.binary== TRUE] <- 1 
IKNL.data$vital_status.binary <- as.numeric(IKNL.data$vital_status.binary)
IKNL.data$gebdat <-as.Date(IKNL.data$gebdat,format="%d/%m/%Y")
IKNL.data$incdat <-as.Date(IKNL.data$incdat,format="%d/%m/%Y")
IKNL.data$vitdat <-as.Date(IKNL.data$vitdat,format="%d/%m/%Y")
IKNL.data$ovldat <-as.Date(IKNL.data$ovldat,format="%d/%m/%Y")
IKNL.data$age_at_incdat <- as.numeric(round(difftime(IKNL.data$incdat,
  IKNL.data$gebdat, units = "days") / 365,1))
IKNL.data$follow_up_time <- difftime(IKNL.data$vitdat,IKNL.data$incdat, 
  units = "days") 
IKNL.data <- IKNL.data %>% filter(!is.na(follow_up_time))

# Exploratory Data Analysis
IKNL.data %>% group_by(gender) %>% summarise(avg=mean(age_at_incdat))
summary(IKNL.data$topo)

IKNL_2 <- IKNL.data %>%
  filter(topo == "C02")
summary(IKNL_2)

# Plot the age distribution of patients
hist(as.numeric(difftime(IKNL_2$ovldat,IKNL_2$incdat, units = "days")), 100)
ggplot(data = IKNL_2, aes(x = follow_up_time)) +
  geom_histogram(binwidth = 200, fill = "purple", col = "white") + 
  ylab("Frequency") + xlab("Follow-up time of tongue cancer patients") 
set.seed(1)
ggplot(data = IKNL_2, aes(x = age_at_incdat)) +
  geom_histogram(binwidth = 2, fill = "purple", col = "white") + 
  ylab("Frequency") + xlab("Age") +
  stat_function(fun = dnorm, args = list(mean = mean(IKNL_2$age_at_incdat), 
  sd = sd(IKNL_2$age_at_incdat)))

# Gender and vital status
vital_table <- with(IKNL_2, table(gender, vital_status))
vital_table
prop.table(vital_table)
prop.table(vital_table, 1)
ggplot(data = IKNL_2, aes(x = gender, fill = vital_status)) +
  geom_bar() + xlab ("Gender") + ylab("Number of patients") +
  scale_fill_discrete(name = "Vital status")

diffgr_table <- with(IKNL_2, table(diffgr, vital_status))
diffgr_table
prop.table(diffgr_table, 1)
ggplot(data = IKNL_2, aes(x = diffgr, fill = vital_status)) +
  geom_bar() +
  scale_fill_discrete(name = "Vital status") + ylab("Number of patients") + 
  xlab ("Differentiation grade")

# Survival analysis

# Age
fit_lgm <- glm(vital_status ~ age_at_incdat, family=binomial(link='logit'), 
  data=IKNL_2)
summary(fit_lgm)
confint(fit_lgm)
c <- exp(cbind(Odd_Ratio = coef(fit_lgm), confint(fit_lgm, level = 0.90)))

# Gender
gender_table <- with(IKNL_2, table(gender, vital_status))
gender_test <- chisq.test(gender_table)
gender_test
gender_table

# Differentiation grade
IKNL_2$follow_up_time <- as.numeric(IKNL_2$follow_up_time)
IKNL_2$diffgr2 <- relevel(IKNL_2$diffgr, ref = "9")
surv_object <- Surv(time = IKNL_2$follow_up_time, event = IKNL_2$vital_status.binary)
fit_C02 <- survfit(surv_object~ diffgr2, data = IKNL_2)
p_C02 <- ggsurvplot(fit_C02, data = IKNL_2, pval = TRUE)
plot(p_C02$plot)

fit.coxph.C02 <- coxph(surv_object ~ diffgr2, data = IKNL_2)
summary(fit.coxph.C02)
ggforest(fit.coxph.C02, data = IKNL_2)

# Age and gender
fit_gender <- survfit(surv_object~ gender, data = IKNL_2)
p_C02 <- ggsurvplot(fit_age, data = IKNL_2, pval = TRUE)
plot(p_C02$plot)

# Prediction on other types of cancer other than tongue cancer

# KNN
IKNL_new <- IKNL.data %>%
  filter(topo != "C02")

set.seed(1)
trn_index = createDataPartition(IKNL_new$vital_status, p = 0.70, list = FALSE)
trn_vital = IKNL_new[trn_index,]
tst_vital = IKNL_new[-trn_index,]
tst_vital$vital_status <- factor(tst_vital$vital_status)
trn_vital$vital_status <- factor(trn_vital$vital_status)

set.seed(1)
vital_knn = train(vital_status ~ (age_at_incdat + gender + diffgr)^3, 
  method = "knn", data = trn_vital, 
  trControl = trainControl(method = "cv", number = 10), na.action = na.exclude)
vital_knn
outcome_knn<- predict(vital_knn, tst_vital)

# Evaluate the lgm model - ConfusionMatrix
outcome <- confusionMatrix(outcome_knn, tst_vital$vital_status)
outcome

set.seed(1)
vital_lgr = train(vital_status ~ (age_at_incdat + gender + diffgr)^3, method = "glm",
                  family = binomial(link = "logit"), data = trn_vital,
                  trControl = trainControl(method = 'cv', number = 5), na.action = na.exclude)

# Evaluate the LGM model
outcome_lgr <- predict(vital_lgr, tst_vital)
lgr_conM <- confusionMatrix(outcome_lgr, tst_vital$vital_status)
lgr_conM

Model <- c("KNN", "LGM")
accu <- c(0.552, 0.5689)
sens <- c (0.4828, 0.4272)
spec <- c(0.6154, 0.6985)
prec <- c(precision(outcome_lgr, tst_vital$vital_status), precision(outcome_knn, 
    tst_vital$vital_status))
df <- data.frame("Model" = model, "Accuracy" = accu, "Sensitivity" = sens, 
  "Specificity" = spec, "Precision" = prec)
df <- df%>%
  gather(Metric, Value, c(Accuracy, Sensitivity, Specificity, Precision))

ggplot(data = df, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "purple") +
  facet_grid(~Model) 





