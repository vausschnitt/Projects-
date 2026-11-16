# =====================================================
# Peak Online Purchase Period - Decision Tree Model
# TFB 2063 Data Science Project
# Target: PeakPeriod
# man dis is hard
# =====================================================

# ---- 1. Load packages ----iuu
library(rpart)
library(rpart.plot)
library(caret)

# ---- 2. Load data ----
raw <- read.csv("C:/UTP_Assignment/Sem_5/Data_Science/Assignment/44samples.csv",
                stringsAsFactors = FALSE, encoding = "UTF-8")

# Sanity check: arknights
cat("Rows read:", nrow(raw), "\n")

# Rename columns
colnames(raw) <- c(
  "Id", "StartTime", "CompletionTime", "Email", "Name",
  "Year", "Gender", "WeeklyFreq", "MonthlyFreq", "ShopDays",
  "TimeBracket", "Platform", "Category", "Spending", "TimingReason",
  "PeakPeriod", "AcademicInfluence_Exam", "AcademicInfluence_Break",
  "AcademicInfluence_Project", "AllowanceEffect", "StressShopping",
  "PurchaseStyle", "DelayedPurchase", "DeliveryImportance"
)

data <- raw

# ---- 3. Clean known data-entry issues ----
# One dude just says stuff?
data$Category[data$Category == "Stuff"] <- NA

# ---- 4. Clean the 1-5 "X - Label" scale columns into plain numbers ----
extract_num <- function(x) as.numeric(gsub("[^0-9].*", "", x))

data$AcademicInfluence_Exam    <- extract_num(data$AcademicInfluence_Exam)
data$AcademicInfluence_Break   <- extract_num(data$AcademicInfluence_Break)
data$AcademicInfluence_Project <- extract_num(data$AcademicInfluence_Project)
data$DeliveryImportance        <- as.numeric(data$DeliveryImportance)

# ---- 4. Handle multi-select columns (semicolon-separated) ----
# ShopDays
days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
for (d in days) {
  data[[paste0("Day_", d)]] <- as.factor(
    ifelse(grepl(d, data$ShopDays), "Yes", "No")
  )
}

# TimeBracket (also multi-select in this dataset gfgfgfg
# more than one time-of-day, e.g. "Evening;Late Night")
brackets <- c("Morning", "Afternoon", "Evening", "Late Night")
bracket_labels <- c("Time_Morning", "Time_Afternoon", "Time_Evening", "Time_LateNight")
for (i in seq_along(brackets)) {
  data[[bracket_labels[i]]] <- as.factor(
    ifelse(grepl(brackets[i], data$TimeBracket), "Yes", "No")
  )
}

# TimingReason
reasons <- c("Availability of funds", "discount vouchers", "Immediate necessity",
             "Academic schedule", "All of the above")
reason_labels <- c("Reason_Funds", "Reason_Discount", "Reason_Necessity",
                   "Reason_Academic", "Reason_All")
for (i in seq_along(reasons)) {
  data[[reason_labels[i]]] <- as.factor(
    ifelse(grepl(reasons[i], data$TimingReason), "Yes", "No")
  )
}

# ---- 5. Set factor types for remaining categorical variables ----
factor_cols <- c("Year", "Gender", "WeeklyFreq", "MonthlyFreq",
                 "Platform", "Category", "Spending", "AllowanceEffect",
                 "StressShopping", "PurchaseStyle", "DelayedPurchase", "PeakPeriod")
data[factor_cols] <- lapply(data[factor_cols], as.factor)

levels(data$PeakPeriod)  # check which categories are present in your data
table(data$PeakPeriod)   # count per category - each should be > 1
# if this looks wrong (e.g. only 1 row total),
# the CSV didn't read correctly - stop and check raw

# ---- 6. Select predictors for the model ----
predictor_cols <- c("Year", "Gender", "WeeklyFreq", "MonthlyFreq",
                    "Platform", "Category", "Spending",
                    "AcademicInfluence_Exam", "AcademicInfluence_Break",
                    "AcademicInfluence_Project", "AllowanceEffect",
                    "StressShopping", "PurchaseStyle", "DelayedPurchase",
                    "DeliveryImportance",
                    paste0("Day_", days), bracket_labels, reason_labels)

model_data <- data[, c("PeakPeriod", predictor_cols)]

# ---- 7. Train/test split ----
set.seed(123)
train_index <- createDataPartition(model_data$PeakPeriod, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

# ---- 8. Build the Decision Tree ----
tree_model <- rpart(PeakPeriod ~ ., data = train_data, method = "class")
rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE,
           varlen = 0, faclen = 0, tweak = 1.1, cex = 0.75,
           box.palette = "auto",
           main = "Decision Tree: Predicting Peak Purchase Period")

# ---- 9. Predict & evaluate ----
predictions <- predict(tree_model, test_data, type = "class")
confusionMatrix(predictions, test_data$PeakPeriod)

# ---- 10. View IF-THEN rules (for your write-up) ----
print(tree_model)

# ---- 11. Recode target into 2 classes ----
model_data$PeakBinary <- as.factor(ifelse(
  grepl("Mega Sales", model_data$PeakPeriod),
  "Mega Sale Period", "Festive Season Period"
))
table(model_data$PeakBinary)  # check the new 2-class balance

# ---- 12. Train/test split (binary target) ----
set.seed(123)
train_index_b <- createDataPartition(model_data$PeakBinary, p = 0.7, list = FALSE)
train_data_b <- model_data[train_index_b, !(names(model_data) %in% "PeakPeriod")]
test_data_b  <- model_data[-train_index_b, !(names(model_data) %in% "PeakPeriod")]

# ---- 13. Build and plot the binary Decision Tree ----
tree_model_b <- rpart(PeakBinary ~ ., data = train_data_b, method = "class")

rpart.plot(tree_model_b, type = 2, extra = 104, fallen.leaves = TRUE,
           varlen = 0, faclen = 0, tweak = 1.1, cex = 0.85,
           box.palette = "auto",
           main = "Decision Tree: Mega Sale vs Festive Season Period")

# ---- 14. Predict & evaluate (binary target) ----
predictions_b <- predict(tree_model_b, test_data_b, type = "class")
confusionMatrix(predictions_b, test_data_b$PeakBinary)

# ---- 15. View IF-THEN rules (binary target) ----
print(tree_model_b)

# ---- Bar chart: Mega Sale Period vs Festive Season Period ----
ggplot(model_data, aes(x = PeakBinary, fill = PeakBinary)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Mega Sale Period" = "#4C72B0",
                               "Festive Season Period" = "#DD8452")) +
  labs(title = "Peak Purchase Period: Mega Sale vs Festive Season",
       x = NULL, y = "Number of Respondents") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# ---- kNN models ----
knn_data <- model_data[, c("PeakBinary", "DeliveryImportance", "AcademicInfluence_Break")]

# kNN requires scaled features - caret handles this via preProcess
ctrl <- trainControl(method = "cv", number = 5)
knn_model <- train(PeakBinary ~ ., data = knn_data,
                   method = "knn",
                   trControl = ctrl,
                   preProcess = c("center", "scale"),
                   tuneGrid = data.frame(k = c(3,5,7,9,11)))

print(knn_model)          # shows accuracy for each k tested
plot(knn_model)           # visualizes accuracy vs k

# Final prediction on holdout test set
knn_pred <- predict(knn_model, newdata = test_data_b)
confusionMatrix(knn_pred, test_data_b$PeakBinary)

# ---- 1. Target distribution: Peak Purchase Period ----
ggplot(data, aes(x = PeakPeriod)) +
  geom_bar(fill = "#4C72B0") +
  labs(title = "Distribution of Peak Purchase Periods",
       x = NULL, y = "Number of Respondents") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# ---- 2. Preferred platform ----
ggplot(data, aes(x = Platform)) +
  geom_bar(fill = "#DD8452") +
  labs(title = "Preferred Online Shopping Platform",
       x = NULL, y = "Number of Respondents") +
  theme_minimal(base_size = 13)

# ---- 3. Product category (pie chart) ----
cat_counts <- as.data.frame(table(data$Category))
colnames(cat_counts) <- c("Category", "Count")
cat_counts$Percent <- round(100 * cat_counts$Count / sum(cat_counts$Count), 0)
cat_counts$Label <- paste0(cat_counts$Percent, "%\n(n=", cat_counts$Count, ")")

ggplot(cat_counts, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3.5) +
  coord_polar("y") +
  labs(title = "Main Product Category Purchased") +
  theme_void(base_size = 13)

# ---- 4. Peak Period by Spending Amount (stacked bar) ----
data$Spending <- factor(data$Spending,
                        levels = c("Below RM 50", "RM 50 - RM 100",
                                   "RM 100 - RM 250", "Above RM 250"))

ggplot(data, aes(x = Spending, fill = PeakPeriod)) +
  geom_bar(position = "stack") +
  labs(title = "Peak Purchase Period by Spending Amount",
       x = "Spending per Transaction", y = "Number of Respondents",
       fill = "Peak Period") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ---- 5. Academic calendar influence (grouped means) ----
influence_summary <- data.frame(
  Period = c("Exam Periods", "Semester Breaks", "Project Submission Weeks"),
  AvgInfluence = c(mean(data$AcademicInfluence_Exam, na.rm = TRUE),
                   mean(data$AcademicInfluence_Break, na.rm = TRUE),
                   mean(data$AcademicInfluence_Project, na.rm = TRUE))
)

ggplot(influence_summary, aes(x = Period, y = AvgInfluence)) +
  geom_bar(stat = "identity", fill = "#55A868") +
  ylim(0, 5) +
  labs(title = "Average Academic Calendar Influence on Shopping",
       x = NULL, y = "Average Influence Score (1-5)") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

#--- scatter plot---
# ---- Scatterplot + Linear Regression: Spending vs Delivery Importance ----
# Convert Spending bracket to a numeric midpoint estimate
data$SpendingMid <- dplyr::case_when(
  data$Spending == "Below RM 50"     ~ 25,
  data$Spending == "RM 50 - RM 100"  ~ 75,
  data$Spending == "RM 100 - RM 250" ~ 175,
  data$Spending == "Above RM 250"    ~ 300,
  TRUE ~ NA_real_
)

ggplot(data, aes(x = SpendingMid, y = DeliveryImportance)) +
  geom_jitter(width = 8, height = 0.15, color = "#4C72B0", alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#C44E52") +
  labs(title = "Spending Amount vs. Delivery Speed Importance",
       x = "Estimated Spending per Transaction (RM)",
       y = "Delivery Speed Importance (1-5)") +
  theme_minimal(base_size = 13)

# Correlation + regression summary for your write-up
cor.test(data$SpendingMid, data$DeliveryImportance)
summary(lm(DeliveryImportance ~ SpendingMid, data = data))

# ---- Boxplot: Delivery Speed Importance by Spending Bracket ----
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

# ---- Heatmap: Shopping Day x Time Bracket ---- fefe
days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
brackets <- c("Morning","Afternoon","Evening","Late Night")

heat_data <- expand.grid(Day = days, Time = brackets)
heat_data$Count <- mapply(function(d, t) {
  sum(grepl(d, data$ShopDays) & grepl(t, data$TimeBracket))
}, heat_data$Day, heat_data$Time)

heat_data$Day <- factor(heat_data$Day, levels = days)
heat_data$Time <- factor(heat_data$Time, levels = brackets)

ggplot(heat_data, aes(x = Day, y = Time, fill = Count)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = Count), size = 4) +
  scale_fill_gradient(low = "#F2F2F2", high = "#4C72B0") +
  labs(title = "Shopping Activity by Day and Time Bracket",
       x = NULL, y = NULL, fill = "Respondents") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ---- Correlation heatmap: numeric influence variables ----
library(reshape2)

num_vars <- data[, c("AcademicInfluence_Exam", "AcademicInfluence_Break",
                     "AcademicInfluence_Project", "DeliveryImportance")]
corr_matrix <- round(cor(num_vars, use = "complete.obs"), 2)
corr_melt <- melt(corr_matrix)

ggplot(corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), size = 4) +
  scale_fill_gradient2(low = "#C44E52", mid = "white", high = "#4C72B0",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Between Numeric Survey Variables",
       x = NULL, y = NULL, fill = "r") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ---- Feature Importance: ----
importance_df <- data.frame(
  Variable = names(tree_model_b$variable.importance),
  Importance = tree_model_b$variable.importance
)
importance_df <- importance_df[order(-importance_df$Importance), ]
importance_df <- head(importance_df, 8)

ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#4C72B0") +
  coord_flip() +
  labs(title = "What Actually Predicts Mega Sale vs. Festive Season Shopping",
       x = NULL, y = "Importance Score") +
  theme_minimal(base_size = 13)

# ---- Lean tree: Timing Reason drivers only (Necessity/Discount/Funds) etc wdwdw ----
reason_predictors <- c("Reason_Funds", "Reason_Discount", "Reason_Necessity", "AllowanceEffect")

train_reason <- train_data_b[, c("PeakBinary", reason_predictors)]
test_reason  <- test_data_b[, c("PeakBinary", reason_predictors)]

tree_reason <- rpart(PeakBinary ~ ., data = train_reason, method = "class")

rpart.plot(tree_reason, type = 2, extra = 104, fallen.leaves = TRUE,
           varlen = 0, faclen = 0, tweak = 1.1, cex = 0.9,
           main = "Purchase Timing Logic: Mega Sale vs Festive Season")

pred_reason <- predict(tree_reason, test_reason, type = "class")
confusionMatrix(pred_reason, test_reason$PeakBinary)

# Compare 
majority_baseline <- max(table(model_data$PeakBinary)) / nrow(model_data)
cat("Majority-class baseline accuracy:", round(majority_baseline, 3), "\n")

# ---- Best-performing lean tree: Delivery Importance + Academic Break Influence ----
final_predictors <- c("DeliveryImportance", "AcademicInfluence_Break")

train_final <- train_data_b[, c("PeakBinary", final_predictors)]
test_final  <- test_data_b[, c("PeakBinary", final_predictors)]

tree_final <- rpart(PeakBinary ~ ., data = train_final, method = "class")

rpart.plot(tree_final, type = 2, extra = 104, fallen.leaves = TRUE,
           varlen = 0, faclen = 0, tweak = 1.1, cex = 0.95,
           main = "Predicting Mega Sale vs Festive Season Shopping")

pred_final <- predict(tree_final, test_final, type = "class")
confusionMatrix(pred_final, test_final$PeakBinary)

# Compare group means (for your write-up)
aggregate(cbind(DeliveryImportance, AcademicInfluence_Break) ~ PeakBinary,
          data = model_data, FUN = mean)

# ---- Scatterplot with Decision Tree boundary overlay ----
ggplot(model_data, aes(x = AcademicInfluence_Break, y = DeliveryImportance,
                       color = PeakBinary)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3, alpha = 0.75) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "gray40") +
  geom_segment(x = 3.5, xend = 5.5, y = 4.5, yend = 4.5,
               linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Mega Sale Period" = "#4C72B0",
                                "Festive Season Period" = "#DD8452")) +
  labs(title = "Predicting Peak Period from Delivery Importance & Academic Influence",
       x = "Academic Influence \u2013 Semester Break (1-5)",
       y = "Delivery Speed Importance (1-5)",
       color = "Peak Period") +
  xlim(0.5, 5.5) + ylim(0.5, 5.5) +
  theme_minimal(base_size = 13)