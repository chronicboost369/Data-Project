# Houseprice_pred
Predictive Model on House Price
This is a predictive model constructed on the house data from [Kaggle](https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data).
If interested, read the pdf report and any comments, suggestions, and questions are welcomed.

The regression methods and other machine learning techniques(or statistical) discussed are: 
1. KNN imputation 
2. multiple linear regression
3. Variable selection
4. Regularization
5. Nonlinearity test
6. Random Forest
7. Interaction

Some future works planned at the moment are:
1. Automation of adding variables with test RMSE ( extension of variable selection)
2. Boostrapping to increase the sample size
3. Cross validations
4. Dimension Reduction via PCA & Autoencoder

## For houseprice.R
### Data Cleaning/Process 
1. Check missing observations
2. Remove categorical variables if levels are too high or unproportional
3. For continuous variables, to treat missing variables, other varaibles highly associted, via correlation, with the targeted variable are selected. Then, use them for KNN imputation to replace missing variables
4. Removed continuous variables for multicollinearity
5. Remove observations if missing values across many categorical variables
6. Remove outliers for the response variable using empirical 90% percentile


### Modelling
1. Start with MLR with all of the variables and review the result
2. Because the residuals are not normally distributed, any p-values or CI are not reliable. Then, derive test RMSE
3. 2 choices: A. Focus on prediction(only test RMSE matters) B. Focus on statistical relationship (must meet normality assumptions)
4. Choice A was chosen b/c using log transform on this data with small observations(total of 1460) will result in a bias estimation when back-transformed to evaluate the magnitude of the test RMSE.
5. To improve test Statistics, used polynomial regression, randomForest for variable selection.

### Future work
1. Robust regression
2. PCA/Autoencoder to reduce the dimension then redo the regression
