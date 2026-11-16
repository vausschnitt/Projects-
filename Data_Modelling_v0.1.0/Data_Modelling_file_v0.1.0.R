# =====================================================
# Peak Online Purchase Period - Decision Tree Model
# TFB 2063 Data Science Project
# Target: PeakPeriod - which festive/sale event drives
#         the student's peak online shopping
# =====================================================

# ---- 1. Load packages ----
# install.packages(c("rpart", "rpart.plot", "caret"))
library(rpart)
library(rpart.plot)
library(caret)

# ---- 2. Load data ----
# EDIT the filename below to match your actual exported CSV
raw <- read.csv("C:/UTP_Assignment/Sem_5/Data_Science/Assignment/44samples.csv",
                stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")

# Rename columns by position (Google Forms headers are the full question
# text, so we replace them with short working names)
colnames(raw) <- c(
  "Id", "StartTime", "CompletionTime", "Email", "Name",
  "Year", "Gender", "WeeklyFreq", "MonthlyFreq", "ShopDays",
  "TimeBracket", "Platform", "Category", "Spending", "TimingReason",
  "PeakPeriod", "AcademicInfluence_Exam", "AcademicInfluence_Break",
  "AcademicInfluence_Project", "AllowanceEffect", "StressShopping",
  "PurchaseStyle", "DelayedPurchase", "DeliveryImportance"
)

data <- raw

# ---- 3. Clean the 1-5 "X - Label" scale columns into plain numbers ----
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
factor_cols <- c("Year", "Gender", "WeeklyFreq", "MonthlyFreq", "TimeBracket",
                 "Platform", "Category", "Spending", "AllowanceEffect",
                 "StressShopping", "PurchaseStyle", "DelayedPurchase", "PeakPeriod")
data[factor_cols] <- lapply(data[factor_cols], as.factor)

levels(data$PeakPeriod)  # check which categories are present in your data

# ---- 6. Select predictors for the model ----
predictor_cols <- c("Year", "Gender", "WeeklyFreq", "MonthlyFreq", "TimeBracket",
                    "Platform", "Category", "Spending",
                    "AcademicInfluence_Exam", "AcademicInfluence_Break",
                    "AcademicInfluence_Project", "AllowanceEffect",
                    "StressShopping", "PurchaseStyle", "DelayedPurchase",
                    "DeliveryImportance",
                    paste0("Day_", days), reason_labels)

model_data <- data[, c("PeakPeriod", predictor_cols)]

# ---- 7. Train/test split ----
set.seed(123)
train_index <- createDataPartition(model_data$PeakPeriod, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

# ---- 8. Build the Decision Tree ----
tree_model <- rpart(PeakPeriod ~ ., data = train_data, method = "class")

rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE,
           main = "Decision Tree: Predicting Peak Purchase Period")

# ---- 9. Predict & evaluate ----
predictions <- predict(tree_model, test_data, type = "class")
confusionMatrix(predictions, test_data$PeakPeriod)

# ---- 10. View IF-THEN rules (for your write-up) ----
print(tree_model)

# NOTE: Your current dataset (30 responses, 4 PeakPeriod categories present)
# is small for a 4-5 class model. If accuracy looks poor or the tree is
# unstable, consider collecting more responses, or simplifying the target
# to 2 classes (e.g. "Mega Sale Period" vs "Festive Season Period").


# =====================================================
# Data Visualization - Online Shopping Behavior Survey
# TFB 2063 Data Science Project
# =====================================================
# Run this AFTER the data-cleaning steps from your decision
# tree script (Steps 1-5), so `data` is already prepared.

# install.packages("ggplot2")
library(ggplot2)

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

# NOTE: Chart 1 shows your target is imbalanced (Mega Sales dominates
# responses) - worth mentioning as a limitation in your write-up.
# Also check the "Stuff" entry under Category (Q8) - it doesn't match
# any of your original form options and may need cleaning.