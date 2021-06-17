data <- diabetes
head(data)
library(VGAM)

#Logit Function

model1 <- glm(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age, data = data, 
              family=binomial("logit"))
summary(model1)
anova(model1)

#Multinomial Logistic Regression Model

model2 <- vglm(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age, data = data, 
               family=multinomial)
summary(model2)

#Poisson Regression Model

model3 <- glm(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+DiabetesPedigreeFunction+Age, data = data, 
              family=poisson)
summary(model3)
anova(model3)

#For Model Selection in R we will compare AIC and BIC values of models.

#AIC values for model1, model2 and model3

AIC(model1)
AIC(model2)
AIC(model3)

# We observe that AIC values for the both model1 and model2 are less when compared with model3. We can also observe that AIC values for both model and model2 are equal.

#BIC values for model1, model2 and model3

BIC(model1)
BIC(model2)
BIC(model3)

# We can see that BIC values for the both model1 and model2 are less when compared with model3. We can also observe that BIC values for both model1 and model2 are equal.

# Hence we can conclude that as AIC and BIC values for model1 i.e., Logistic Regression model and model2 i.e., Multinomial Logistic Regression Model are less we would go with either model1 or model2.
