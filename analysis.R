install.packages("lmtest")
library(lmtest)

#Read in the data
data <- read.csv("C:/Users/sushm/Downloads/sgdata.csv")


# Step 2: Check for missing values in the entire dataset
sum(is.na(data))  # Check total missing values in the dataset
# Log transform income
data$log_income <- log(data$Income)

# Convert categorical variables to factors
data$Sex <- factor(data$Sex, levels = c(0, 1), labels = c("Female", "Male"))
data$Marital.status <- factor(data$Marital.status)
data$Education <- factor(data$Education, 
                         levels = c("high school", "other / unknown", "university", "graduate school"),
                         ordered = TRUE)
data$Occupation <- factor(data$Occupation)
data$Settlement.size <- factor(data$Settlement.size, levels = c(0, 1, 2), 
                               labels = c("Small", "Medium", "Large"))

# Clean column names
names(data) <- gsub(" ", "_", names(data))

# Create numeric versions of education and occupation for IV analysis
data$education_num <- as.numeric(data$Education)
data$occupation_num <- as.numeric(data$Occupation)

# 4. MULTIPLE REGRESSION MODELS
# ===========================
# 3. OLS Regression (Without Addressing Endogeneity)
# Run a basic OLS regression with education and occupation as predictors of income
# Model 3: Log income on education and occupation
ols_model <- lm(log_income ~ Education + Occupation, data = data)
cat("\nModel 3: Log income on education and occupation\n")
print(summary(ols_model))

# 2. NAIVE OLS REGRESSION (POTENTIALLY BIASED)
# =========================================
naive_model <- lm(log_income ~ education_num + occupation_num + Sex + Age + Marital.status, 
                  data = data)
cat("\nNaive OLS Regression (potentially biased):\n")
print(summary(naive_model))


# Residual plot
plot(naive_model$residuals, main = "Residuals Plot", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")  # Add a horizontal line at 0

# Run the Ramsey RESET test to detect omitted variable bias
resettest(naive_model)

# 3. MANUAL TWO-STAGE LEAST SQUARES (2SLS)
# =====================================

# First stage regressions
# Regress endogenous variables (education_num and occupation_num) on instruments and exogenous variables
first_stage_edu <- lm(education_num ~ Settlement.size + Sex + Age + Marital.status, 
                      data = data)
cat("\nFirst Stage Regression for Education:\n")
print(summary(first_stage_edu))

first_stage_occ <- lm(occupation_num ~ Settlement.size + Sex + Age + Marital.status,
                      data = data)
cat("\nFirst Stage Regression for Occupation:\n")
print(summary(first_stage_occ))

# Test instrument relevance using F-statistics
edu_fstat <- summary(first_stage_edu)$fstatistic
occ_fstat <- summary(first_stage_occ)$fstatistic

cat("\nInstrument Relevance:\n")
cat("F-statistic for education first stage:", edu_fstat[1], "\n")
cat("F-statistic for occupation first stage:", occ_fstat[1], "\n")
cat("(F > 10 indicates a strong instrument)\n")

# Get predicted values from first stage
data$edu_hat <- predict(first_stage_edu)
data$occ_hat <- predict(first_stage_occ)

# Second stage regression
# Use predicted values from first stage
iv_model <- lm(log_income ~ edu_hat + occ_hat + Sex + Age + Marital.status,
               data = data)
cat("\nSecond Stage (IV) Regression:\n")
print(summary(iv_model))

# 4. HAUSMAN TEST FOR ENDOGENEITY
# =============================
# Add residuals from first stage to OLS model
data$edu_resid <- residuals(first_stage_edu)
data$occ_resid <- residuals(first_stage_occ)

hausman_test <- lm(log_income ~ education_num + occupation_num + Sex + Age + 
                     Marital.status + edu_resid + occ_resid,
                   data = data)
cat("\nHausman Test for Endogeneity:\n")
print(summary(hausman_test))

# Extract p-values for residuals
edu_pval <- summary(hausman_test)$coefficients["edu_resid", 4]
occ_pval <- summary(hausman_test)$coefficients["occ_resid", 4]

cat("\nEndogeneity Test Results:\n")
cat("Education residual p-value:", edu_pval, 
    ifelse(edu_pval < 0.05, " (significant - endogeneity present)", " (not significant)"), "\n")
cat("Occupation residual p-value:", occ_pval, 
    ifelse(occ_pval < 0.05, " (significant - endogeneity present)", " (not significant)"), "\n")

# 5. COMPARE OLS AND IV ESTIMATES
# ============================
# Gather coefficients for comparison
coef_comparison <- data.frame(
  Variable = c("Education", "Occupation"),
  OLS = c(coef(naive_model)["education_num"], coef(naive_model)["occupation_num"]),
  IV = c(coef(iv_model)["edu_hat"], coef(iv_model)["occ_hat"]),
  Difference = c(
    coef(iv_model)["edu_hat"] - coef(naive_model)["education_num"],
    coef(iv_model)["occ_hat"] - coef(naive_model)["occupation_num"]
  ),
  Percent_Change = c(
    (coef(iv_model)["edu_hat"] - coef(naive_model)["education_num"]) / coef(naive_model)["education_num"] * 100,
    (coef(iv_model)["occ_hat"] - coef(naive_model)["occupation_num"]) / coef(naive_model)["occupation_num"] * 100
  )
)

cat("\nComparison of OLS and IV Estimates:\n")
print(coef_comparison)

# 8. ENDOGENEITY TESTS
# ==================
cat("\nPerforming endogeneity tests...\n")

# Add residuals from first stage to OLS model
data$edu_resid <- residuals(first_stage_edu)
data$occ_resid <- residuals(first_stage_occ)

hausman_test <- lm(log_income ~ education_num + occupation_num + Sex + Age + 
                     Marital.status + edu_resid + occ_resid,
                   data = data)
cat("\nEndogeneity test results (Hausman approach):\n")
print(summary(hausman_test))

# Extract p-values for residuals
edu_pval <- summary(hausman_test)$coefficients["edu_resid", 4]
occ_pval <- summary(hausman_test)$coefficients["occ_resid", 4]

cat("\nP-value for education residual:", edu_pval, 
    "\nP-value for occupation residual:", occ_pval, "\n")
cat("(P-value < 0.05 indicates endogeneity)\n")


# 6. REDUCED FORM REGRESSION
# =======================
# Direct effect of instrument on outcome
reduced_form <- lm(log_income ~ Settlement.size + Sex + Age + Marital.status,
                   data = data)
cat("\nReduced Form Regression (Direct effect of instrument on outcome):\n")
print(summary(reduced_form))

# 7. ROBUSTNESS CHECKS
# =================
# IV model with different control variables
iv_robust1 <- lm(log_income ~ edu_hat + occ_hat + Sex + Age,
                 data = data)
cat("\nIV Model with Different Controls:\n")
print(summary(iv_robust1))

# IV model with education only as endogenous variable
edu_only_iv <- lm(log_income ~ edu_hat + occupation_num + Sex + Age + Marital.status,
                  data = data)
cat("\nIV Model with Education Only as Endogenous:\n")
print(summary(edu_only_iv))

# IV model with occupation only as endogenous variable
occ_only_iv <- lm(log_income ~ education_num + occ_hat + Sex + Age + Marital.status,
                  data = data)
cat("\nIV Model with Occupation Only as Endogenous:\n")
print(summary(occ_only_iv))

# 8. INCOME INEQUALITY IMPLICATIONS
# ==============================
# Custom function for coefficient of variation
cv <- function(x) {
  sd(x) / mean(x)
}

# Custom function for 90/10 ratio
p90_p10 <- function(x) {
  quantile(x, 0.9) / quantile(x, 0.1)
}

# Generate predicted incomes from different models
data$pred_naive <- exp(predict(naive_model))
data$pred_iv <- exp(predict(iv_model))

# Calculate inequality measures
actual_cv <- cv(data$Income)
naive_cv <- cv(data$pred_naive)
iv_cv <- cv(data$pred_iv)

actual_p90_p10 <- p90_p10(data$Income)
naive_p90_p10 <- p90_p10(data$pred_naive)
iv_p90_p10 <- p90_p10(data$pred_iv)

# Compare inequality measures
inequality_comparison <- data.frame(
  Model = c("Actual", "OLS", "IV"),
  CV = c(actual_cv, naive_cv, iv_cv),
  P90_P10 = c(actual_p90_p10, naive_p90_p10, iv_p90_p10)
)

cat("\nInequality Measure Comparison:\n")
print(inequality_comparison)

# Between-group inequality decomposition
decompose_between_group <- function(income, group) {
  # Calculate between-group component
  group_means <- tapply(income, group, mean)
  group_sizes <- table(group)
  group_weights <- group_sizes / sum(group_sizes)
  
  overall_mean <- mean(income)
  between_component <- sum(group_weights * ((group_means - overall_mean)^2) / overall_mean^2)
  
  return(between_component)
}

# Calculate between-group inequality by education
edu_between_actual <- decompose_between_group(data$Income, data$Education)
edu_between_naive <- decompose_between_group(data$pred_naive, data$Education)
edu_between_iv <- decompose_between_group(data$pred_iv, data$Education)

# Calculate between-group inequality by occupation
occ_between_actual <- decompose_between_group(data$Income, data$Occupation)
occ_between_naive <- decompose_between_group(data$pred_naive, data$Occupation)
occ_between_iv <- decompose_between_group(data$pred_iv, data$Occupation)

# Compare decomposition results
decomp_comparison <- data.frame(
  Group = c("Education", "Occupation"),
  Actual = c(edu_between_actual, occ_between_actual),
  OLS = c(edu_between_naive, occ_between_naive),
  IV = c(edu_between_iv, occ_between_iv)
)

cat("\nBetween-Group Inequality Decomposition:\n")
print(decomp_comparison)

# 9. SUMMARY OF FINDINGS
# ===================
cat("\n=== SUMMARY OF FINDINGS ===\n")

cat("\n1. Instrument Relevance:\n")
cat("   F-statistic for education first stage:", edu_fstat[1], "\n")
cat("   F-statistic for occupation first stage:", occ_fstat[1], "\n")
cat("   Conclusion: Settlement size appears to be a ")
if (edu_fstat[1] > 10 && occ_fstat[1] > 10) {
  cat("strong instrument for both education and occupation.\n")
} else if (edu_fstat[1] > 10) {
  cat("strong instrument for education but weaker for occupation.\n")
} else if (occ_fstat[1] > 10) {
  cat("strong instrument for occupation but weaker for education.\n")
} else {
  cat("relatively weak instrument, which could limit causal inference.\n")
}

cat("\n2. Endogeneity Test:\n")
cat("   Education residual p-value:", edu_pval, "\n")
cat("   Occupation residual p-value:", occ_pval, "\n")
cat("   Conclusion: ")
if (edu_pval < 0.05 || occ_pval < 0.05) {
  cat("Endogeneity is present, making the IV approach more appropriate.\n")
} else {
  cat("Endogeneity may not be a major concern in this dataset.\n")
}

cat("\n3. Causal Effects:\n")
cat("   Education effect (OLS):", coef_comparison$OLS[1], "\n")
cat("   Education effect (IV):", coef_comparison$IV[1], 
    " (", sprintf("%+.1f", coef_comparison$Percent_Change[1]), "%)\n", sep="")
cat("   Occupation effect (OLS):", coef_comparison$OLS[2], "\n")
cat("   Occupation effect (IV):", coef_comparison$IV[2], 
    " (", sprintf("%+.1f", coef_comparison$Percent_Change[2]), "%)\n", sep="")

cat("\n4. Inequality Implications:\n")
cat("   Coefficient of Variation (Actual):", actual_cv, "\n")
cat("   Coefficient of Variation (OLS predicted):", naive_cv, "\n")
cat("   Coefficient of Variation (IV predicted):", iv_cv, "\n\n")
cat("   90/10 Ratio (Actual):", actual_p90_p10, "\n")
cat("   90/10 Ratio (OLS predicted):", naive_p90_p10, "\n")
cat("   90/10 Ratio (IV predicted):", iv_p90_p10, "\n")

cat("\n5. Between-Group Inequality:\n")
cat("   Education explains", round(edu_between_actual*100, 1), 
    "% of actual income inequality\n")
cat("   After IV correction, education explains", round(edu_between_iv*100, 1), 
    "% of income inequality\n\n")
cat("   Occupation explains", round(occ_between_actual*100, 1), 
    "% of actual income inequality\n")
cat("   After IV correction, occupation explains", round(occ_between_iv*100, 1), 
    "% of income inequality\n")

cat("\n6. Final Conclusion:\n")
cat("   The causal effect of education on income is")
if (abs(coef_comparison$Percent_Change[1]) > 20) {
  cat(" substantially")
} else if (abs(coef_comparison$Percent_Change[1]) > 5) {
  cat(" moderately")
} else {
  cat(" slightly")
}
if (coef_comparison$IV[1] > coef_comparison$OLS[1]) {
  cat(" higher")
} else {
  cat(" lower")
}
cat(" than naive estimates suggest.\n")

cat("   The causal effect of occupation on income is")
if (abs(coef_comparison$Percent_Change[2]) > 20) {
  cat(" substantially")
} else if (abs(coef_comparison$Percent_Change[2]) > 5) {
  cat(" moderately")
} else {
  cat(" slightly")
}
if (coef_comparison$IV[2] > coef_comparison$OLS[2]) {
  cat(" higher")
} else {
  cat(" lower")
}
cat(" than naive estimates suggest.\n")

cat("\nInstrumental Variable Analysis completed successfully.\n")



# Step 10: Residual Analysis - Run a basic regression
lm_model <- lm(Income ~ Education + Age + Occupation, data = data_clean)

# Residuals analysis (visualize residuals)
residuals <- lm_model$residuals
plot(residuals, main = "Residuals Plot", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")  # Add a horizontal line at 0

# Step 11: Adding Control Variables - Check for OVB by adding more variables
lm_model_with_controls <- lm(Income ~ Education + Age + Occupation, data = data_clean)
summary(lm_model_with_controls)

# Step 12: Ramsey RESET Test for Model Misspecification
resettest(lm_model)

# Step 13: Run IV regression using Settlement_size as an instrument for Education
iv_model <- ivreg(Income ~ Education | Settlement.size, data = data_clean)
summary(iv_model)

# Step 14: Multicollinearity Check - VIF (Variance Inflation Factor)
vif(lm_model_with_controls)

# Step 15: First-Stage Regression (Check if Settlement_size is a valid instrument for Education)
first_stage <- lm(Education ~ Settlement.size, data = data_clean)
summary(first_stage)

# Step 16: Check for correlation between Education and Settlement_size
cor(data_clean$Education, data_clean$Settlement.size, use = "complete.obs")

Check the correlation between income, education, occupation, and settlement size
cor(data[c("Income", "Education", "Occupation", "Settlement.size")], use = "complete.obs")

# 2. OLS Regression (Without Addressing Endogeneity)
# Run a basic OLS regression with education and occupation as predictors of income
ols_model <- lm(Income ~ Education + Occupation, data = data)
summary(ols_model)

# 3. Instrumental Variables (2SLS)
# Stage 1: Regress education and occupation on settlement size (instrument)
stage1_education <- lm(Education ~ Settlement.size, data = data)
stage1_occupation <- lm(Occupation ~ Settlement.size, data = data)

# Display results for Stage 1
summary(stage1_education)
summary(stage1_occupation)

# Stage 2: Use the predicted values from stage 1 to predict income
data$predicted_education <- predict(stage1_education)
data$predicted_occupation <- predict(stage1_occupation)

# 7. Additional Control Variables (Optional)
# If you have control variables (e.g., Age, Sex, Marital Status), you can add them to the models
ols_with_controls <- lm(Income ~ Education + Occupation + Age + Sex + Marital.status, data = data)
iv_with_controls <- ivreg(Income ~ Education + Occupation + Age + Sex + Marital.status | Settlement.size, data = data)

# Display results for OLS and IV models with controls
summary(ols_with_controls)
summary(iv_with_controls)

# 8. Pretty Output for Regression Results
# You can use stargazer for a nicer presentation of the regression results
stargazer(ols_model, iv_model, ols_with_controls, iv_with_controls, type = "text")


