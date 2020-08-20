## Economic Forecasting

### Content

1. Forecasting Trend: when a time series evolves slowly and smoothly over time, we say that it shows a trend. (long-term behavior for the entire series e.g. 10 years)
3. Forecasting Cycles: when a time series exhibit periodic fluctuations, we say that it has a cycle. (something repeats itself over many years)
3. Forecasting Seasonality: a cycle is *seasonal* when specific fluctuations occur within the calender year, for instance activites that peak in summer months.
4. Forecasting with Regression Models (with time-series data)
5. Combining Forecasts (competing models)
6. Advanced Methods (e.g. ARIMA, GARCH, etc.)

#### What is a forecast?

* A forecast is a statement about the future.

* > We define **forecasting** as the *science* and the *art* to predict a *future* event with some degree of accuracy.

#### How is forecasting done by economists?

* In economics, methods of forecasting include:
  * Guessing, "rule of thumb", or "informal models"
  * Expert judgment
  * Extrapolation
  * Leading indicators
  * Surveys
  * Time series models
  * Econometric systems

#### What are the main problems?

* One of the main problems with forecasting in economics is that economies evolve over time and are subject to intermittent, and sometimes large, unanticipated shocks.


#### *What are some relevant forecast examples?*

* Some relevant areas:
  * Operations planning and control
  * Marketing
  * Economics
  * Financial asset management
  * Financial risk management
  * ...

### **Standard Notation**

* $\{y_t \}$ entire set of observations
* $y_t$ known value of the series
* $Y_{t+h}$ random variable (future at time t+h)
* $y_{t+h}$ unknown value of the random variable
* $I_t$ univariate / multivariate information set
* $f_{t,1}$ 1-step ahead (tomorrow based on today)
* $f_{t,h}$ h-step ahead ()
* $e_ {t,h}= y_ {t+h} - f_ {t,h}$

### Model Building Strategy

* Q: How to find an appropriate model for a Time Series?
* A: There are 3 steps
  1. Model Specification (or identification)
     * Select the types of plausible models given the data
  2. Model Fitting
     * Follow the parsimony principle
  3. Model Diagnostics
     * Assess the quality of the model







