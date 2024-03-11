# Exploratory-Data-Analysis-For-Housing-Price-Prediction-in-Melbourne

**Description:**
This repository presents an exploratory data analysis (EDA) conducted on the Melbourne housing [dataset](https://www.kaggle.com/datasets/dansbecker/melbourne-housing-snapshot) sourced from Kaggle. The analysis aims to determine the optimal parameters for predicting housing unit prices in Melbourne, leveraging techniques in regression analysis and statistical testing.

**Key Highlights:**
- **Dataset Overview:** The dataset consists of 13,580 rows and 21 columns, with four main variables under consideration: Price (in Australian dollars), Distance (from central business district), Propertycount (number of properties in the suburb), and Landsize (in meters).

- **Data Cleaning and Preparation:** The data underwent preprocessing, including outlier removal using the Interquartile Range (IQR) method and subsequent transformations for better fit, such as log transformation on Price and Landsize, and square root transformation on Propertycount and Distance.

- **Data Analysis:** A linear regression model was employed to analyze the relationship between housing prices and key predictor variables such as distance, property count, and land size. Additionally, variable selection techniques were utilized to identify the most influential parameters.

- **Model Evaluation:** Assumptions including multicollinearity, independence of residuals, and normal distribution of residuals were validated. The all-subsets method was utilized for variable selection, leading to the identification of the best-fit model. Additionally, comparison among different models was conducted using ANOVA and AIC, with Model2 (incorporating all three predictor variables) identified as the superior model.

- **Conclusion:** The analysis culminates in the identification of a robust linear model for predicting housing prices in Melbourne. While the model meets assumptions and provides valuable insights, there remains potential for improvement through the incorporation of additional variables.

**Dataset Citation:**
- Kaggle. "Melbourne Housing Snapshot." Available at: [Dataset Link](https://www.kaggle.com/datasets/dansbecker/melbourne-housing-snapshot)
