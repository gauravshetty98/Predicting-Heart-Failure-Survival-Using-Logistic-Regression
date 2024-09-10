# Installing required packages
install.packages("corrplot")
library(corrplot)


# Loading the data
health_data = read.csv('Patient_details.csv')
head(health_data)

# Checking if there are any null values. As you can see the dataset does not have any null values
null_values <- is.na(health_data)
summary(null_values)

#Creating the correlation plot
correlation_matrix <- cor(health_data)
corrplot(correlation_matrix, type = "lower", tl.corner = names(health_data))
# from the correlation plot we can see that the following variables have the most correlation:
# age, ejection_fraction, serum_creatinine, serum_sodium, time



# Creating full model first
mdl1= glm( DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + diabetes + 
             ejection_fraction + high_blood_pressure + platelets + serum_creatinine + 
             serum_sodium + sex + smoking + time, data = health_data, family = binomial)
summary(mdl1)
anova(mdl1)
# the residual deviance is 219.55 on 286 degrees of freedom


#Testing goodness of fit 
DevianceRes=sum(residuals(mdl1, type = "deviance")^2)
DevianceRes
pchisq(DevianceRes, df=286, lower.tail=FALSE)  
# COMPUTED P-VALUE IS 0.99
# Since the p-value is high and deviance/df is lower than 1 we can say that the model is a good fit to the data

PearSonRes=sum(residuals(mdl1, type = "pearson")^2)
PearSonRes
pchisq(PearSonRes, df=286, lower.tail=FALSE)  
# COMPUTED P-VALUE IS 0.874
# Since the p-value is high and pearson/df is lower than 1 we can say that the model is a food fit to the data


#TESTING  OVERALL MODEL
chiSq_stat=mdl1$null.deviance-mdl1$deviance
chiSq_stat

dif_df= mdl1$df.null-mdl1$df.residual
dif_df

pvalue=1-pchisq(mdl1$null.deviance-mdl1$deviance, mdl1$df.null-mdl1$df.residual)
pvalue
#P-value is extremely low which suggests that the model is significant

#scatterplot for time vs death event
health_data$death_event_f = as.factor(health_data$DEATH_EVENT)

ggplot(health_data, aes(x = time, y = age)) +
  geom_point(aes(color = factor(death_event_f)))

#Without time variable:
health_data_wot <- health_data[,c(1,2,3,4,5,6,7,8,9,10,11,13)]
mdl_wt= glm( DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + diabetes + 
             ejection_fraction + high_blood_pressure + platelets + serum_creatinine + 
             serum_sodium + sex + smoking, data = health_data_wot, family = binomial)
summary(mdl_wt)
anova(mdl_wt)

#TESTING  OVERALL MODEL
chiSq_stat_wt=mdl_wt$null.deviance-mdl_wt$deviance
chiSq_stat_wt

dif_df_wt= mdl_wt$df.null-mdl_wt$df.residual
dif_df_wt

pvalue=1-pchisq(mdl_wt$null.deviance-mdl_wt$deviance, mdl_wt$df.null-mdl_wt$df.residual)
pvalue
#P-value is extremely low which suggests that the model is significant


# Model Selection:
#Stepwise selection
stepwise_model_wt <- stepAIC(mdl_wt)
summary(stepwise_model_wt)

#Forward selection
initial_model_wt <- glm( formula = DEATH_EVENT ~ 1, data = health_data_wot, family = binomial)
forward_model_wt <- step(initial_model_wt, direction = "forward", scope = list(lower = initial_model_wt, upper = mdl_wt))
summary(forward_model_wt)
# We get the same model in forward selection


#Backward selection
backward_model_wt <- step(mdl_wt)
summary(backward_model_wt)
#Age, anaemia, creatinine_phosphokinase, ef, hbp, sc, ss 
# residual_dev: 296.05

#Odds ratio to get an inference of the model
OR=exp(coef(stepwise_model_wt)) ######## ODDS RATIO OF THE VARIABLES
OR

#Predicting based on our model and creating the confusion matrix
probs = predict(stepwise_model_wt, health_data[,c(1,2,3,4,5,6,7,8,9,10,11)], type = "response")
probs  
threshold <- 0.5
predictions <- ifelse(probs > threshold, 1, 0)

confusion_matrix <- table(health_data$DEATH_EVENT, predictions)
confusion_matrix

accuracy = (182 + 45)/299
recall = (182)/(182+21)
precision = (182)/(182+51)
f1_score = (precision*recall)/(precision+recall)

