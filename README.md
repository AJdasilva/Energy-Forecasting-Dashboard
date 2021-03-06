## Energy Forcasting Dashboard
Dashboard for Energy Forcasting for a Single Home Using Gradient Boosting and R
***

### What is this app?
<p>
An interactive analytics and visualization tool for assessing how variations in a set of parameters affect the predictive accuracy of solar power generation and power consumption forecasting models applied to a single home. There are four main tabs on this dashboard: instructional (this page), summary statistics, power consumption and solar power generation. The power consumption and solar power generation pages both include a plot of the test data, a table of predictions on the test data which includes the actual values for comparison, the mean squared error of the model on the test data, and a control panel used to adjust various parameters (listed below). The summary statistics page contains basic information and visualizations of the dataset being used.
</p>
<p>
The models used in this app are known as gradient boosting models or GBMs. Intuitively, a GBM is a machine learning model which builds an ensemble of decision trees in a step-wise fashion so that the latter models learn from the mistakes of the prior models. 
</p>
<p>
The adjustable parameters on the power consumption and solar power generation tabs include:
</p>

<p><ul><li>	<b>Forecast Horizon:</b> length of time into the future where the prediction is made </li></ul><p>
<p><ul><li>	<b>Analysis Range:</b> any window of time from January 1st, 2014 - December 15th, 2016 to use for training and testing data </li></ul></p>
<p><ul><li>	<b>Date to End Training:</b> date which splits analysis range into training and testing data </li></ul></p>
<p><ul><li> <b>Time Period:</b> range of hours during the day for which the predictions are made </li></ul></p>
<p><ul><li>	<b>Viewing Window Range:</b> any window of time within the test data range </li></ul></p>

<p>
The forecasting models use a fixed number of predictors variables which are related to weather and electricity. The variables are summarised below.
</p>
<p>
<b><ul><li>Predictor variables used in demand prediction:</b> 
	current day's power use (over specified time period),    exponentially-weighted moving average of use over previous n days (n is forecast     horizon), current day's humidity and temperature, day of the week, season and month.
</ul></li></p>
<p><ul><li>
<b>Predictor variables used in solar prediction:</b>
	current day's power generation (over specified time period), exponentially-weighted moving average of generation over previous n days (n is forecast horizon), season month, and current day's humidity, temperature, visibility, wind bearing, pressure, precipitation and cloud-cover.
</li></ul></p>
<b>Parameters used in the gradient boosting model:</b>
	500 trees, interaction depth of 5, and a shrinkage parameter of 0.01.

<b>Dataset information:</b> The data used in this dashboard is from a single home's electricity consumption and solar generation data; weather data is also used to complement the electrical data. Data is from a collection of datasets called the Smart* Data Set for Sustainability  which is part of the Umass Trace Repository. Specifically, we used Home C from the 2017 release of the dataset.

### Who made it?

This dashboard is a collaborative effort between Lance Wrobel, Aaron Da Silva, and Rose Dennis.
