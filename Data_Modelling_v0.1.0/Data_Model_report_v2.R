# =====================================================================
# Predicting Peak Online Purchase Periods Among UTP Students.
# TFB 2063 Data Science
# Am, Des , Bin
# =====================================================================

library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)
library(reshape2)


# ---- Import

raw <- read.csv("C:/UTP_Assignment/Sem_5/Data_Science/Assignment/44samples.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
cat("Rows read:", nrow(raw), "\n")  # sanity check - should print 44

colnames(raw) <- c(
  "Id", "StartTime", "CompletionTime", "Email", "Name",
  "Year", "Gender", "WeeklyFreq", "MonthlyFreq", "ShopDays",
  "TimeBracket", "Platform", "Category", "Spending", "TimingReason",
  "PeakPeriod", "AcademicInfluence_Exam", "AcademicInfluence_Break",
  "AcademicInfluence_Project", "AllowanceEffect", "StressShopping",
  "PurchaseStyle", "DelayedPurchase", "DeliveryImportance"
)
data <- raw

#a dude just say stuff in the survey 
data$Category[data$Category == "Stuff"] <- NA

# Convert labelled Likert-scale text into plain numeric scores.
extract_num <- function(x) as.numeric(gsub("[^0-9].*", "", x))
data$AcademicInfluence_Exam    <- extract_num(data$AcademicInfluence_Exam)
data$AcademicInfluence_Break   <- extract_num(data$AcademicInfluence_Break)
data$AcademicInfluence_Project <- extract_num(data$AcademicInfluence_Project)
data$DeliveryImportance        <- as.numeric(data$DeliveryImportance)

# Expand multi-select fields into binary
# Yes/No indicator columns.
days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
for (d in days) {
  data[[paste0("Day_", d)]] <- as.factor(
    ifelse(grepl(d, data$ShopDays), "Yes", "No"))
}

brackets <- c("Morning", "Afternoon", "Evening", "Late Night")
bracket_labels <- c("Time_Morning", "Time_Afternoon", "Time_Evening", "Time_LateNight")
for (i in seq_along(brackets)) {
  data[[bracket_labels[i]]] <- as.factor(
    ifelse(grepl(brackets[i], data$TimeBracket), "Yes", "No"))
}

reasons <- c("Availability of funds", "discount vouchers", "Immediate necessity",
             "Academic schedule", "All of the above")
reason_labels <- c("Reason_Funds", "Reason_Discount", "Reason_Necessity",
                   "Reason_Academic", "Reason_All")
for (i in seq_along(reasons)) {
  data[[reason_labels[i]]] <- as.factor(
    ifelse(grepl(reasons[i], data$TimingReason), "Yes", "No"))
}

# Set remaining categorical variables as factors
factor_cols <- c("Year", "Gender", "WeeklyFreq", "MonthlyFreq", "Platform",
                 "Category", "Spending", "AllowanceEffect", "StressShopping",
                 "PurchaseStyle", "DelayedPurchase", "PeakPeriod")
data[factor_cols] <- lapply(data[factor_cols], as.factor)

# Simplify the target into 2 classes 
data$PeakBinary <- as.factor(ifelse(
  grepl("Mega Sales", data$PeakPeriod),
  "Mega Sale Period", "Festive Season Period"
))
table(data$PeakBinary)

baseline_acc <- max(table(data$PeakBinary)) / nrow(data)
cat("Majority-class baseline accuracy:", round(baseline_acc, 3), "\n")


# ----: Initial full-predictor model (exploratory) ----

predictor_cols_full <- c("Year", "Gender", "WeeklyFreq", "MonthlyFreq",
                         "Platform", "Category", "Spending",
                         "AcademicInfluence_Exam", "AcademicInfluence_Break",
                         "AcademicInfluence_Project", "AllowanceEffect",
                         "StressShopping", "PurchaseStyle", "DelayedPurchase",
                         "DeliveryImportance",
                         paste0("Day_", days), bracket_labels, reason_labels)

model_data_full <- data[, c("PeakBinary", predictor_cols_full)]

set.seed(123)
train_index <- createDataPartition(model_data_full$PeakBinary, p = 0.7, list = FALSE)
train_full <- model_data_full[train_index, ]
test_full  <- model_data_full[-train_index, ]

tree_full <- rpart(PeakBinary ~ ., data = train_full, method = "class")
pred_full <- predict(tree_full, test_full, type = "class")
confusionMatrix(pred_full, test_full$PeakBinary)

sort(tree_full$variable.importance, decreasing = TRUE)


# ----: Cross-validated single-predictor screening ----
# 5- fold sumting

ctrl <- trainControl(method = "cv", number = 5)
cv_results <- data.frame(Predictor = character(), Accuracy = numeric())

for (v in predictor_cols_full) {
  f <- as.formula(paste("PeakBinary ~", v))
  m <- tryCatch(
    train(f, data = data, method = "rpart", trControl = ctrl),
    error = function(e) NULL
  )
  if (!is.null(m)) {
    cv_results <- rbind(cv_results,
                        data.frame(Predictor = v, Accuracy = max(m$results$Accuracy)))
  }
}

cv_results <- cv_results[order(-cv_results$Accuracy), ]
print(cv_results)
cat("\nBaseline to beat:", round(baseline_acc, 3), "\n")


# --- Final, leaner predictive model ----

final_predictors <- c("DeliveryImportance", "AcademicInfluence_Break")
model_data_final <- data[, c("PeakBinary", final_predictors)]

set.seed(123)
train_index_f <- createDataPartition(model_data_final$PeakBinary, p = 0.7, list = FALSE)
train_final <- model_data_final[train_index_f, ]
test_final  <- model_data_final[-train_index_f, ]

tree_final <- rpart(PeakBinary ~ ., data = train_final, method = "class")

rpart.plot(tree_final, type = 2, extra = 104, fallen.leaves = TRUE,
           varlen = 0, faclen = 0, tweak = 1.1, cex = 0.95,
           main = "Final Decision Tree: Mega Sale vs Festive Season Period")

pred_final <- predict(tree_final, test_final, type = "class")
confusionMatrix(pred_final, test_final$PeakBinary)

# Cross-validated accuracy of the final model
cv_final <- train(PeakBinary ~ ., data = model_data_final,
                  method = "rpart", trControl = ctrl)
print(cv_final)

print(tree_final)  # IF-THEN rules for the report


# #####################################################################
# SECTION H — RESULT AND FINDING am part
# #####################################################################

# ---descriptive Analysi ----
# before simplificatio Target distribution: Peak Purchase Period ----
ggplot(data, aes(x = PeakPeriod)) +
  geom_bar(fill = "#4C72B0") +
  labs(title = "Distribution of Peak Purchase Periods",
       x = NULL, y = "Number of Respondents") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# Figure 1: Shopping day frequency
ggplot(data, aes(x = factor(1))) +  # placeholder not used; see below
  theme_void()

day_counts <- data.frame(
  Day = days,
  Count = sapply(days, function(d) sum(grepl(d, data$ShopDays)))
)
day_counts$Day <- factor(day_counts$Day, levels = days)

ggplot(day_counts, aes(x = Day, y = Count)) +
  geom_col(fill = "#4C72B0") +
  labs(title = "Frequency of Online Shopping by Day of Week (n=44)",
       x = NULL, y = "Number of Respondents") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# Figure 2: Time bracket frequency
time_counts <- data.frame(
  Bracket = brackets,
  Count = sapply(brackets, function(b) sum(grepl(b, data$TimeBracket)))
)
time_counts$Bracket <- factor(time_counts$Bracket, levels = brackets)

ggplot(time_counts, aes(x = Bracket, y = Count)) +
  geom_col(fill = "#DD8452") +
  labs(title = "Frequency of Online Shopping by Time of Day (n=44)",
       x = NULL, y = "Number of Respondents") +
  theme_minimal(base_size = 13)

# Figur 3: Day x Time heatmap
heat_data <- expand.grid(Day = days, Time = brackets)
heat_data$Count <- mapply(function(d, t) {
  sum(grepl(d, data$ShopDays) & grepl(t, data$TimeBracket))
}, heat_data$Day, heat_data$Time)
heat_data$Day  <- factor(heat_data$Day, levels = days)
heat_data$Time <- factor(heat_data$Time, levels = brackets)

ggplot(heat_data, aes(x = Day, y = Time, fill = Count)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = Count), size = 4) +
  scale_fill_gradient(low = "#F2F2F2", high = "#4C72B0") +
  labs(title = "Shopping Activity by Day and Time Bracket",
       x = NULL, y = NULL, fill = "Respondents") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Fige 4: Peak period target (binary)
ggplot(data, aes(x = PeakBinary, fill = PeakBinary)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Mega Sale Period" = "#4C72B0",
                               "Festive Season Period" = "#DD8452")) +
  labs(title = "Peak Purchase Period: Mega Sale vs Festive Season",
       x = NULL, y = "Number of Respondents") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")


# -Diagnostic Analysis ----
# ---- Bar chart: Planned vs Impulsive Purchasing & Delayed Purchase Behaviour dwdwa----
behaviour_summary <- data.frame(
  Question = c("Purchase Style", "Purchase Style",
               "Delayed for Deals", "Delayed for Deals"),
  Response = c("Long research (Planned)", "Impulsive (Immediate)",
               "Yes", "No"),
  Count = c(sum(data$PurchaseStyle == "Long research (Planned)"),
            sum(data$PurchaseStyle == "Impulsive (Immediate)"),
            sum(data$DelayedPurchase == "Yes"),
            sum(data$DelayedPurchase == "No"))
)
behaviour_summary$Percent <- round(100 * behaviour_summary$Count /
                                     ave(behaviour_summary$Count, behaviour_summary$Question, FUN = sum))

ggplot(behaviour_summary, aes(x = Question, y = Count, fill = Response)) +
  geom_col(position = "stack") +
  geom_text(aes(label = paste0(Percent, "%\n(n=", Count, ")")),
            position = position_stack(vjust = 0.5), size = 3.8, color = "white") +
  scale_fill_manual(values = c("Long research (Planned)" = "#4C72B0",
                               "Impulsive (Immediate)"   = "#B8C4DE",
                               "Yes" = "#DD8452",
                               "No"  = "#F2D0B8")) +
  labs(title = "Purchasing Behaviour: Planned Research vs Impulsive Buying",
       x = NULL, y = "Number of Respondents", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# Purchase style / delayed purchase 
table(data$PurchaseStyle)
table(data$DelayedPurchase)

# Boxplot: Delivery Importance by Spending Amount
data$Spending <- factor(data$Spending,
                        levels = c("Below RM 50", "RM 50 - RM 100",
                                   "RM 100 - RM 250", "Above RM 250"))

ggplot(data, aes(x = Spending, y = DeliveryImportance, fill = Spending)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1.5) +
  labs(title = "Delivery Speed Importance by Spending Amount",
       x = "Spending per Transaction", y = "Delivery Speed Importance (1-5)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 15, hjust = 1))

# Academic calendar influence 
influence_summary <- data.frame(
  Period = c("Exam Periods", "Semester Breaks", "Project Submission Weeks"),
  AvgInfluence = c(mean(data$AcademicInfluence_Exam, na.rm = TRUE),
                   mean(data$AcademicInfluence_Break, na.rm = TRUE),
                   mean(data$AcademicInfluence_Project, na.rm = TRUE))
)

ggplot(influence_summary, aes(x = Period, y = AvgInfluence)) +
  geom_col(fill = "#55A868") +
  ylim(0, 5) +
  labs(title = "Average Academic Calendar Influence on Shopping",
       x = NULL, y = "Average Influence Score (1-5)") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))


aggregate(cbind(DeliveryImportance, AcademicInfluence_Break) ~ PeakBinary,
          data = model_data_final, FUN = mean)
#----kNN comparison model----
knn_model <- train(PeakBinary ~ ., data = model_data_final,
                   method = "knn",
                   trControl = ctrl,
                   preProcess = c("center", "scale"),
                   tuneGrid = data.frame(k = c(3, 5, 7, 9, 11)))
print(knn_model)
plot(knn_model)

knn_pred <- predict(knn_model, newdata = test_final)
confusionMatrix(knn_pred, test_final$PeakBinary)

# - Predictive Analysis ----

# ---- Strip plot: single-predictor split (DeliveryImportance < 5) ----
ggplot(model_data_final, aes(x = DeliveryImportance, y = PeakBinary, color = PeakBinary)) +
  geom_jitter(width = 0.1, height = 0.15, size = 3, alpha = 0.75) +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "gray40") +
  annotate("text", x = 4.5, y = 2.4, label = "Split: DeliveryImportance < 5",
           hjust = 0.5, size = 3.5, color = "gray30") +
  scale_color_manual(values = c("Mega Sale Period" = "#4C72B0",
                                "Festive Season Period" = "#DD8452")) +
  scale_x_continuous(breaks = 1:5) +
  labs(title = "Final Decision Tree Split: Delivery Importance Predicts Peak Period",
       x = "Delivery Speed Importance (1-5)", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# ---- Predicted vs Actual: model correctness by respondent ----
model_data_final$Predicted <- ifelse(model_data_final$DeliveryImportance < 5,
                                     "Festive Season Period", "Mega Sale Period")
model_data_final$Correct <- model_data_final$Predicted == as.character(model_data_final$PeakBinary)

ggplot(model_data_final, aes(x = DeliveryImportance, y = PeakBinary,
                             color = PeakBinary, shape = Correct)) +
  geom_jitter(width = 0.1, height = 0.15, size = 3.5, alpha = 0.85) +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "gray40") +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 4),
                     labels = c("TRUE" = "Correctly classified",
                                "FALSE" = "Misclassified")) +
  scale_color_manual(values = c("Mega Sale Period" = "#4C72B0",
                                "Festive Season Period" = "#DD8452")) +
  scale_x_continuous(breaks = 1:5) +
  labs(title = "Predicted vs Actual: Model Correctness by Respondent",
       x = "Delivery Speed Importance (1-5)", y = NULL,
       color = "Actual Class", shape = "Prediction") +
  theme_minimal(base_size = 13)

# ---- Break down misclassifications by error type ----
results_df <- data.frame(
  Actual = test_final$PeakBinary,
  Predicted = pred_final
)

false_negatives <- results_df[results_df$Actual == "Mega Sale Period" &
                                results_df$Predicted == "Festive Season Period", ]
false_positives <- results_df[results_df$Actual == "Festive Season Period" &
                                results_df$Predicted == "Mega Sale Period", ]

cat("False Negatives (missed Mega Sale shoppers):", nrow(false_negatives), "\n")
cat("False Positives (wrongly flagged as Mega Sale):", nrow(false_positives), "\n")

# ---- Full prediction statistics for the final model ----tst

# 1. Basic confusion matrix + overall stats 
cm <- confusionMatrix(pred_final, test_final$PeakBinary)
print(cm)

# 2. Just the confusion matrix 
cm$table

# 3. Overall accuracy, with 95% confidence interval
cm$overall

# 4. Class-level stats: Precision, Recall (Sensitivity), Specificity, F1
cm$byClass

# 5. Cleaner, labeled breakdown 
stats_summary <- data.frame(
  Metric = c("Accuracy", "Sensitivity (Recall)", "Specificity",
             "Precision (Pos Pred Value)", "F1 Score", "Balanced Accuracy"),
  Value = c(
    round(cm$overall["Accuracy"], 3),
    round(cm$byClass["Sensitivity"], 3),
    round(cm$byClass["Specificity"], 3),
    round(cm$byClass["Pos Pred Value"], 3),
    round(cm$byClass["F1"], 3),
    round(cm$byClass["Balanced Accuracy"], 3)
  )
)
print(stats_summary)

# 6. Manual sanity check - count correct/incorrect directly
n_correct <- sum(pred_final == test_final$PeakBinary)
n_total   <- length(pred_final)
cat("Correct:", n_correct, "/", n_total,
    "=", round(n_correct/n_total, 3), "\n")

# 7. Baseline comparison (always guessing majority class)
baseline_acc <- max(table(test_final$PeakBinary)) / nrow(test_final)
cat("Baseline (majority class) accuracy:", round(baseline_acc, 3), "\n")
cat("Improvement over baseline:", round(cm$overall["Accuracy"] - baseline_acc, 3), "\n")


