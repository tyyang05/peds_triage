import xgboost as xgb
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, cohen_kappa_score

def xgboost_standard(data):
    X=data.drop(columns=['subject_id','stay_id', 'chiefcomplaint', 'arrival', 'acuity'])
    Y=data['acuity']
    Y_shifted = Y - 1 #the triage levels range from 1 to 5 but we need 0 to 4 for the model
    X_train, X_test, y_train, y_test = train_test_split(X, Y_shifted, test_size = 0.2, random_state = 0, stratify = Y_shifted)
    classifier = xgb.XGBClassifier(n_estimators=500, learning_rate=0.05,
                                   objective='multi:softmax',
                                   num_class=4, #this is the number of triage levels
                                   eval_metric='mlogloss',
                                   early_stopping_rounds=25,
                                   random_state=0)
    classifier.fit(X_train, y_train, eval_set=[(X_test, y_test)])
    predicted = classifier.predict(X_test)
    print(classification_report(y_test, predicted))
    print(cohen_kappa_score(y_test, predicted, weights='quadratic')) #gives a measure of how off the model is

def xgboost_regression(data):
    X = data.drop(columns=['subject_id', 'stay_id', 'chiefcomplaint', 'arrival', 'acuity'])
    Y = data['acuity']
    X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size=0.2, random_state=0, stratify=Y)
    classifier = xgb.XGBRegressor(n_estimators=500, learning_rate=0.05,
                                   objective='reg:squarederror',
                                  early_stopping_rounds = 25,
                                   random_state=0)
    classifier.fit(X_train, y_train, eval_set=[(X_test, y_test)])
    predicted = classifier.predict(X_test)
    rounded = np.round(predicted).astype(int)
    rounded = np.clip(rounded, 1, 5) #round only to one of the valid triage outputs
    print(classification_report(y_test, rounded))
    print(cohen_kappa_score(y_test, rounded, weights='quadratic'))


if __name__ == '__main__':
    #this file contains only the tabular quantitative data with no text features
    processed_data = pd.read_excel("MIMIC_Dataset.xlsx")
    #xgboost_standard(processed_data)
    xgboost_regression(processed_data)

