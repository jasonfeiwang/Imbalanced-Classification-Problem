# Binary Classification Problem with highly imbalanced class-distribution
This project aims to classify the income level (under 50K or above 50K) of a U.S. worker based on the 1995 census data. The minority class is less than 6% of the totol datasets and thus challenges the accuracy of the binary classifier. 

I took these three steps:
 
Step 1: identify the best accuracy metrics for modeling performance testing 
TPR (True Positive Rate) and
TNR (True Negative Rate)

Step 2: run a logistic classifier with original dataset of imbalanced class-distribution.
 
Step 3: repeat Step 2 with the following re-sampling techniques:
1. Undersampling the majority class
2. Oversampleing the minority class
3. Synthetic Minority Oversampleing Technique (SMOTE)
4. Random Over Sampling Examples (ROSE)

The conclusion is that the ROSE technique leads to the optimal classification result.
With ROSE I increased the TPR from 0.31 to 0.89, while maintaining the same TNR, comparing to the original classifier. 
