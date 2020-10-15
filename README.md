3 Team members

Regression Analysis to create a predictive model to explain median value of homes in Boston.

**Note** : Median housing values are used because they are more accurate as compared to mean values. Median values are not affected by outliers.

We have used **506 observations** and **13 predictors** (features) for prediction.

The predictors (**X values**) are:\
**1** per capita crime rate by town     
**2** proportion of residential land zoned for lots over 25,000 sq.ft.   
**3** proportion of non-retail business acres per town  
**4** Charles River dummy variable   
**5** nitric oxide concentration (parts per 10 million)   
**6** average number of rooms per dwelling  
**7** proportion of owner occupied units built prior to 1940   
**8** weighted distances to five Boston employment centres  
**9** index of accessibility to radial highways   
**10** full-value property-tax rate per 10,000   
**11** pupil-teacher ratio by town   
**12** 1000(B − 0.63)2 where B is the proportion of African Americans by town   
**13** a numeric vector of percentage values of lower status population 

**Preparing the Dataset**     
Dataset is divided into training data (model building dataset) and testing data.  

**Exploratory Data Analysis (EDA)**
Vizualisations are used to perform EDA to *eliminate variables* not impacting Median Housing Value (target variable).   

   - Visualizations used:     
      - **Summary Statistic** (to find a central measure and detect outliers)     
      - **Correlation between** Median Value and feature (to eliminated uncorrelated features)        
      - **Box Plot** (to detect outliers)    
      - **Heat Map**

**Model Selection and Validation** where the correct model is identified.

**Diagnostocs**

   - Outlying Y observations detected
   - Outlying X observations detected
   - Influencial Observations detected
  
 
 
   


    

 


