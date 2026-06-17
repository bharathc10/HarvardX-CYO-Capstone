# ---- Libraries ----

library(tidyverse)
library(readxl)

options(timeout = 120)

# ---- Data Acquisition ----

zip_file   <- "online_retail.zip"
excel_file <- "Online Retail.xlsx"

if (!file.exists(zip_file)) {
  download.file(
    "https://archive.ics.uci.edu/static/public/352/online+retail.zip",
    zip_file, mode = "wb"
  )
}

if (!file.exists(excel_file)) {
  unzip(zip_file, excel_file)
}

retail_data <- read_excel(excel_file)

# ---- Initial Exploration ----

glimpse(retail_data)
summary(retail_data)

# Raw financial baseline before any filtering — used later to measure how much each
# cleaning step chips away at total value
total_sale_value <- retail_data %>%
  summarise(Total_Sale_Value = sum(Quantity * UnitPrice)) %>%
  pull(Total_Sale_Value)

# Unfiltered AOV — this will look inflated once we strip cancellations and adjustments
aov_overall <- retail_data %>%
  summarise(AOV_Overall = sum(Quantity * UnitPrice) / n_distinct(InvoiceNo)) %>%
  pull(AOV_Overall)

overall_summary <- retail_data %>%
  summarise(
    Distinct_Orders    = n_distinct(InvoiceNo),
    Distinct_Products  = n_distinct(StockCode),
    Distinct_Customers = n_distinct(CustomerID),
    Total_Sale_Value   = sum(Quantity * UnitPrice),
    AOV_Overall        = sum(Quantity * UnitPrice) / n_distinct(InvoiceNo)
  )

# UK dominates — over 84% of rows and value. Sticking to UK removes currency
# noise and cross-border logistics from the equation
sales_by_country <- retail_data %>%
  summarise(
    Distinct_Orders    = n_distinct(InvoiceNo),
    Distinct_Products  = n_distinct(StockCode),
    Distinct_Customers = n_distinct(CustomerID),
    Total_Sale_Value   = sum(Quantity * UnitPrice),
    AOV                = sum(Quantity * UnitPrice) / n_distinct(InvoiceNo),
    .by = Country
  ) %>%
  mutate(Pct_Sale_Contribution = round(100 * Total_Sale_Value / sum(Total_Sale_Value), 2)) %>%
  relocate(AOV, .after = Pct_Sale_Contribution) %>%
  arrange(desc(Total_Sale_Value))

# 24.93% of rows have no CustomerID — anonymous guest checkouts most likely.
# They're dropped for customer-level work but still represent ~15% of revenue,
# so they're worth noting rather than silently discarding
missing_id_pct <- 100 * sum(is.na(retail_data$CustomerID)) / nrow(retail_data)

na_ids_sale_value <- retail_data %>%
  filter(is.na(CustomerID)) %>%
  summarise(Total_Sale_Value = sum(Quantity * UnitPrice)) %>%
  pull(Total_Sale_Value)

na_value_ratio <- na_ids_sale_value / total_sale_value

# StockCode length flags non-product rows — standard items are 5 to 7 characters.
# Shorter or longer codes are postage, bank charges, manual adjustments, etc.
retail_data <- retail_data %>%
  mutate(
    nChar_StockCode = str_length(StockCode),
    nChar_InvoiceNo = str_length(InvoiceNo)
  ) %>%
  relocate(nChar_StockCode, .after = StockCode) %>%
  relocate(nChar_InvoiceNo, .after = InvoiceNo)

# Full structural breakdown — lets us see the exact value footprint of
# cancellations (C-prefixed invoices) and negative quantity rows together
hierarchy_audit <- retail_data %>%
  mutate(
    Code_Length    = str_length(StockCode),
    Is_Negative    = Quantity < 0,
    Has_C_Invoice  = str_detect(InvoiceNo, "^C")
  ) %>%
  group_by(Code_Length, Is_Negative, Has_C_Invoice) %>%
  summarise(
    Total_Lines = n(),
    Total_Value = sum(Quantity * UnitPrice, na.rm = TRUE),
    Unique_SKUs = n_distinct(StockCode),
    .groups = "drop"
  )

# ---- Clean Analytical Universe ----

cleaned_retail <- retail_data %>%
  mutate(StockCode = tolower(StockCode)) %>%
  filter(Country == "United Kingdom") %>%
  filter(!is.na(CustomerID)) %>%
  filter(Quantity > 0, UnitPrice > 0) %>%
  filter(nChar_StockCode %in% c(5, 6, 7))

cat("Original rows:", nrow(retail_data), "\n")
cat("Cleaned rows: ", nrow(cleaned_retail), "\n")

# ---- Model 1: Customer RFM Segmentation ----

snapshot_date <- max(as_date(retail_data$InvoiceDate), na.rm = TRUE) + days(1)

rfm_data <- retail_data %>%
  mutate(
    StockCode       = tolower(StockCode),
    nChar_StockCode = str_length(StockCode)
  ) %>%
  filter(Country == "United Kingdom") %>%
  filter(!is.na(CustomerID)) %>%
  filter(Quantity > 0, UnitPrice > 0) %>%
  filter(nChar_StockCode %in% c(5, 6, 7) | str_detect(StockCode, "dcgs")) %>%
  mutate(Line_Total = Quantity * UnitPrice) %>%
  group_by(CustomerID) %>%
  summarise(
    Recency   = as.numeric(snapshot_date - max(as_date(InvoiceDate))),
    Frequency = n_distinct(InvoiceNo),
    Monetary  = sum(Line_Total),
    .groups   = "drop"
  )

# RFM distributions are heavily right-skewed — log transform before scaling
# to stop a handful of whales from pulling the cluster centroids
rfm_transformed <- rfm_data %>%
  mutate(
    Log_Recency   = log(Recency),
    Log_Frequency = log(Frequency),
    Log_Monetary  = log(Monetary)
  ) %>%
  mutate(
    Scale_Recency   = as.vector(scale(Log_Recency)),
    Scale_Frequency = as.vector(scale(Log_Frequency)),
    Scale_Monetary  = as.vector(scale(Log_Monetary))
  )

set.seed(42)
matrix_for_kmeans <- rfm_transformed %>%
  select(Scale_Recency, Scale_Frequency, Scale_Monetary)

# Elbow scan — checking where adding more clusters stops meaningfully reducing WCSS
wcss <- vector("numeric", length = 10)
for (k in 1:10) {
  kmeans_model <- kmeans(matrix_for_kmeans, centers = k, nstart = 25, iter.max = 50)
  wcss[k] <- kmeans_model$tot.withinss
}

set.seed(42)
customer_kmeans <- kmeans(matrix_for_kmeans, centers = 3, nstart = 25, iter.max = 50)

rfm_segmented <- rfm_transformed %>%
  mutate(Cluster = as.factor(customer_kmeans$cluster))

cluster_profile <- rfm_segmented %>%
  group_by(Cluster) %>%
  summarise(
    Customer_Count = n(),
    Avg_Recency    = mean(Recency),
    Avg_Frequency  = mean(Frequency),
    Avg_Monetary   = mean(Monetary),
    .groups = "drop"
  )

print("--- Customer Cluster Profiles ---")
print(cluster_profile)

# ---- Why Customer Clusters Aren't Enough ----
#
# The RFM model groups customers by how much they spend overall, which works
# for CRM but tells warehouse teams almost nothing useful.
#
# Two buyers can have identical Monetary scores while being completely different
# operationally: one is a B2B wholesaler buying 2,000 cheap novelty bags in a
# single invoice; the other is a boutique buyer picking up 2 premium leather items.
# Same score, completely different storage and replenishment requirements.
#
# To get something actionable for inventory, we shift the aggregation axis from
# CustomerID to StockCode — profiling each product on its own commercial behavior
# rather than who bought it.

# ---- Model 2: Product-Level Segmentation ----

product_matrix <- retail_data %>%
  mutate(
    StockCode       = tolower(StockCode),
    nChar_StockCode = str_length(StockCode)
  ) %>%
  filter(Country == "United Kingdom") %>%
  filter(!is.na(CustomerID)) %>%
  filter(Quantity > 0, UnitPrice > 0) %>%
  filter(nChar_StockCode %in% c(5, 6, 7) | str_detect(StockCode, "dcgs")) %>%
  mutate(Line_Total = Quantity * UnitPrice) %>%
  group_by(StockCode) %>%
  summarise(
    Total_Quantity = sum(Quantity),
    Total_Revenue  = sum(Line_Total),
    Unique_Buyers  = n_distinct(CustomerID),
    Avg_Price      = mean(UnitPrice),
    .groups = "drop"
  )

# Same skew problem as RFM — a few bestsellers dwarf everything else.
# Log + scale before feeding into K-Means
product_transformed <- product_matrix %>%
  mutate(
    Log_Quantity = log(Total_Quantity),
    Log_Revenue  = log(Total_Revenue),
    Log_Buyers   = log(Unique_Buyers),
    Log_Price    = log(Avg_Price)
  ) %>%
  mutate(
    Scale_Quantity = as.vector(scale(Log_Quantity)),
    Scale_Revenue  = as.vector(scale(Log_Revenue)),
    Scale_Buyers   = as.vector(scale(Log_Buyers)),
    Scale_Price    = as.vector(scale(Log_Price))
  )

set.seed(42)
product_kmeans_matrix <- product_transformed %>%
  select(Scale_Quantity, Scale_Revenue, Scale_Buyers, Scale_Price)

product_wcss <- vector("numeric", length = 10)
for (k in 1:10) {
  km_prod_model <- kmeans(product_kmeans_matrix, centers = k, nstart = 25, iter.max = 100)
  product_wcss[k] <- km_prod_model$tot.withinss
}

print("--- Product WCSS by K ---")
print(tibble(K = 1:10, Product_WCSS = product_wcss))

set.seed(42)
final_product_kmeans <- kmeans(product_kmeans_matrix, centers = 5, nstart = 25, iter.max = 100)

product_segmented <- product_matrix %>%
  mutate(Product_Cluster = as.factor(final_product_kmeans$cluster))

product_profiles <- product_segmented %>%
  group_by(Product_Cluster) %>%
  summarise(
    Product_Count  = n(),
    Mean_Quantity  = round(mean(Total_Quantity), 1),
    Mean_Revenue   = round(mean(Total_Revenue), 2),
    Mean_Buyers    = round(mean(Unique_Buyers), 1),
    Mean_Avg_Price = round(mean(Avg_Price), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_Revenue))

print("--- Product Cluster Profiles ---")
print(product_profiles)

# Cluster interpretation at K = 5:
#
#   Cluster 1 — Core Drivers: high volume, high revenue, wide buyer reach
#               → keep perpetually stocked; short picking paths
#
#   Cluster 4 — Bulk Commodities: high volume but thin margins (~£0.87/unit)
#               → cross-dock rather than store; run bundle promotions
#
#   Cluster 3 — Premium Items: expensive (~£6.91), low volume, decent revenue
#               → price-protect; secure storage; target VIP lists
#
#   Cluster 2 — Slow Cheap Essentials: low price, low volume, few buyers
#               → bundle as add-ons; don't reorder aggressively
#
#   Cluster 5 — Dead Stock: premium-priced but barely sold (~9 units, ~3 buyers)
#               → stop reordering; clear with steep discounts immediately

# ---- Validation ----

anova_quantity <- aov(Total_Quantity ~ Product_Cluster, data = product_segmented)
anova_revenue  <- aov(Total_Revenue  ~ Product_Cluster, data = product_segmented)
anova_price    <- aov(Avg_Price      ~ Product_Cluster, data = product_segmented)

cat("\n--- ANOVA: Volume ---\n");  print(summary(anova_quantity))
cat("\n--- ANOVA: Revenue ---\n"); print(summary(anova_revenue))
cat("\n--- ANOVA: Price ---\n");   print(summary(anova_price))

# Semantic cross-reference — does the word "BAG" mean the same thing to the warehouse
# as it does to K-Means? Spoiler: no, and that's exactly the point
catalog_lookup <- cleaned_retail %>%
  distinct(StockCode, Description) %>%
  group_by(StockCode) %>%
  slice(1) %>%
  ungroup()

product_validation_universe <- product_segmented %>%
  left_join(catalog_lookup, by = "StockCode") %>%
  mutate(Description_Clean = toupper(coalesce(Description, "UNKNOWN UNLABELLED ASSET")))

target_keywords <- c("MUG", "BAG", "HEART", "LUNCH", "SIGN")

semantic_skew_audit <- product_validation_universe %>%
  filter(str_detect(Description_Clean, paste(target_keywords, collapse = "|"))) %>%
  mutate(Keyword_Bucket = case_when(
    str_detect(Description_Clean, "MUG")   ~ "MUG",
    str_detect(Description_Clean, "BAG")   ~ "BAG",
    str_detect(Description_Clean, "HEART") ~ "HEART",
    str_detect(Description_Clean, "LUNCH") ~ "LUNCH",
    str_detect(Description_Clean, "SIGN")  ~ "SIGN",
    TRUE ~ "OTHER"
  )) %>%
  group_by(Keyword_Bucket, Product_Cluster) %>%
  summarise(
    SKU_Count      = n(),
    Average_Volume = round(mean(Total_Quantity), 1),
    Average_Spend  = round(mean(Total_Revenue), 2),
    Average_Cost   = round(mean(Avg_Price), 2),
    .groups        = "drop"
  ) %>%
  arrange(Keyword_Bucket, Product_Cluster)

cat("\n--- Semantic Keyword Skew Audit ---\n")
print(semantic_skew_audit)