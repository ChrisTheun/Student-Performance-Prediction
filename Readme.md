This repository stores the R and Python code that I used to predict the performance of student at the UK's Open University with data from the Open University Learning Analytics Dataset (OULAD) retrieved from: https://analyse.kmi.open.ac.uk/open_dataset.

A detailed description of this project can be found in my online portfolio (https://christheunissen.github.io/ChrisTheunissen/Student%20Performance%20Prediction.html)

In short, I use students' clicks in the Virtual Learning Environment, the number of assessments submitted and the average assessment score to predict whether students will pass or fail their course at 10 different points in the course.
I compare the predictive performance of long short-term memory networks (LSTMs) and traditional machine learning models such as decision trees, logistic regression models, multi-layered perceptrons, random forests, support vector machines, and naive Bayes classifiers.

The first part of the data transformation process is handled in R, while Python is used for the remaining data preparation as well as the modeling.

In the data transformation the following files can be found:

- Data Visualization.R: R file with visualizations made to gain more insights into the OULAD data.
- Data Transformation.R: R file in which the OULAD data is cleaned, transformed and the necessary features are retrieved.
- Data Preparation.ipynb: Jupyter notebook in which the data from the R file above is prepared to serve as input for the LSTM models
- Reformat data for traditional ML models.ipynb: Jupyter notebook in which the data from the Jupyter notebook above is prepared to serve as input for the traditional machine learning models

After running these files, the models can be trained and tested. In the modeling folder there is a Jupyter notebook for each traditional machine learning model:

- Decision Trees.ipynb
- Logistic Regression.ipynb
- MLP.ipynb
- Naive Bayes.ipynb
- Random Forest.ipynb
- SVM.ipynb

as well as a jupyter notebook for each LSTM corresponding to one of the three courses and decile.