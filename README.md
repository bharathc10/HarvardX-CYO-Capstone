# HarvardX-CYO-Capstone
## Online Retail Segmentation Framework

This repository contains the Capstone project submission focusing on machine learning frameworks for customer behavior and inventory operational taxonomy.

### Submission Files

* **Report.Rmd**: The complete R Markdown source document detailing the step-by-step diagnostic audit, modeling methodology, and visual classification process.
* **Report.pdf**: The finalized, compiled PDF publication report containing the complete written analysis and plots.
* **Rscript.r**: The standalone production R script containing the raw execution pipeline. Running this script sequentially outputs the dataset partitions, runs the K-Means models, and prints the final segment metrics directly to the console.

### Final Model Performance

* **Selected Model Architecture**: Double Layered K-Means (K=3 Customer RFM Cohorts + K=5 Operational Product Clusters)
* **Statistical Validity**: Confirmed via ANOVA testing across all 5 inventory groups ($p < 0.001$ for Quantity, Revenue, and Unit Price).
* **Operational Validation**: Confirmed via Semantic Keyword Skew Heuristic, proving that behavioral clustering overrides static text labeling rules for inventory management.
