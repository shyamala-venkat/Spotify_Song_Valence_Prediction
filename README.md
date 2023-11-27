### Linear Regression - Predicting valence score of Spotify songs

The dataset used was taken from the Kaggle platform , https://www.kaggle.com/datasets/yamaerenay/spotify-dataset-19212020-600k-tracks?select=tracks.csv. This dataset consists of records related to various songs from the Spotify platform. The song features were analyzed to gain useful insights about various songs. 

## Below steps were applied on the dataset:

1. Data Cleaning and Data Analysis:
	Exploratory data analysis was performed on the dataset using R and graphs such as histograms, correlation plots, box plots were generated to study the distribution of features, 	detect outlier values and find positively or negatively correlated features.
	Univariate, Bivariate and Multivariate analysis were done by generating graphs such as barplots, stacked bar plots, tree chart, sun burst chart, line chart, radar chart, donut 	chart, pie chart.
2. Hypothesis testing:
	Hypothesis testing methods such as one sample and two sample T tests were used to validate several claims related to the songs.
3. Model Building:
	Linear regression model was built to predict the song valence score, a measure of happiness in the songs using several numerical features of the songs.
	Forward selection method was used to select the best features which significantly contributed in the prediction. R2 metric was used to validate the accuracy of the model.

Results:	 							
The below insights were generated after analyzing the Spotify dataset: 								
	The song writers prefer to compose the songs in major mode than the minor mode as the no. of songs in major mode is greater that of minor mode over the years 1930-2021.								
	The keys C and G are the most used keys for composing the songs in the decades 1930s - 2020s.							
 	The average duration or length of the songs is expected to approach below 3.5 mins as there is a downward trend observed in the last 20 years 2000-2021.							
	Most popular songs are slightly shorter in duration than the least popular songs. This highlights a fact that the attention span of the average music consumer is falling and it 	suggests that shorter songs have a higher chance of gaining more attention and can reach the majority of the audience.								
	Music has become more energetic and loud over time.							
	Electronic instruments are mostly used in the songs than the acoustic instruments.								
	The song positivity (song valence) is decreasing from the decade 1980s.
	The speechiness attribute of the songs is increasing in the last 20 years and this indicates the rise in the number of audio podcasts in the Spotify platform.
	A common pattern observed in the top 1000 popular songs released in the years 2000-2021 : high energy, danceability,loudness , less speechiness, liveness, acousticness and neutral valence score.

Hypothesis testing was conducted and the below conclusion were drawn:
	One sample t-test conclusions:
	■ The mean tempo level of the songs is not equal to 120 Beats per minute. 
	Two sample t-test conclusions:
	The mean tempo levels of the songs vary in major mode songs and minor mode songs. The tempo level of the major mode songs is found to be slightly greater than the tempo level of the minor mode songs.
	Songs in major mode do sound much happier than the songs in minor mode.This can be attributed to the fact that the songs played in major mode are usually happier,merrier, cheerful tunes and hence a slight increase in valence level can be observed. 
 
Linear regression was used to build a model to predict the song valence based on various song features:
	Multiple models were created to predict the song valence score with different combinations of input variables. The best set of input variables to the model was found using the 	regsubsets() function of the leaps package in R.
	The residuals were plotted for the model and the residual error was very less in predicting the target variable. The model with highest R2 score was chosen for the prediction.
