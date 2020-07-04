# Import required libraries
import pandas as pd
import numpy as np 
import matplotlib.pyplot as plt
import sklearn
import tensorflow as tf

# Import necessary modules
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error
from math import sqrt

# Keras specific
import keras
from keras.models import Sequential
from keras.layers import Dense

df = pd.read_csv('evaluations.csv', sep=';', dtype=np.float32) 
print(df.shape)
df.describe()
print(df.dtypes)

target_column = ['Score']
predictors = list(set(list(df.columns))-set(target_column))
df[predictors] = df[predictors]/df[predictors].max()
df.describe()

X = df[predictors].values
y = df[target_column].values

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.30, random_state=40)
print(X_train.shape); print(X_test.shape)

# Define model
model = Sequential()
model.add(Dense(500, input_dim=84, activation= "relu"))
model.add(Dense(200, activation= "relu"))
model.add(Dense(100, activation= "relu"))
model.add(Dense(50, activation= "relu"))
model.add(Dense(1))
model.summary() #Print model Summary

model.compile(loss= "mean_squared_error" , optimizer="adam", metrics=["mean_squared_error"])
model.fit(X_train, y_train, epochs=10)

pred_train= model.predict(X_train)
print(np.sqrt(mean_squared_error(y_train,pred_train)))

pred= model.predict(X_test)
print(np.sqrt(mean_squared_error(y_test,pred))) 

model.save('nn')

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.30, random_state=40)
print(X_train.shape); print(X_test.shape)


loaded_model = tf.keras.models.load_model('nn')
pred_train= loaded_model.predict(X_train)
print(np.sqrt(mean_squared_error(y_train,pred_train)))

pred= loaded_model.predict(X_test)
print(np.sqrt(mean_squared_error(y_test,pred))) 