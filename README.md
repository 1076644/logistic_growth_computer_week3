## Q1) Annotate the README.md file in your logistic_growth repo with more detailed information about the analysis. Add a section on the results and include the estimates for N0, r and K (mention which *.csv file you used).

The data from the file 'experiments.csv' shows data obtained from test tube measurements of a test tube with 900 μl of growth media, and an isolate of the bacterium Escherichia coli suspended in 100 μl of the same media. Logisitcal growth in bacteria can be desrcibed by the following equation:

```math
\begin{equation}
\frac{dN}{dt} = N r (1 - \frac{N}{K})
\end{equation}
```

This represents the rate of change we may see in population growth of E.coli which is then eventually scaled when the population reaches a certain number in the population known as the carrying capacity (K). 

The first section of the analysis is splitting the growth of the bacteria into its distinct stages. In a logisitical model, we see a lag phase, exponential phase and then a stationary phase specifically for this data set. We can visualise the growth using the following code:

``` {r}
growth_data <- read.csv("experiment.csv")

install.packages("ggplot2")
library(ggplot2)

ggplot(aes(t,N), data = growth_data) +
  
  geom_point() +
  
  xlab("t") +
  
  ylab("y") +
  
  theme_bw()
```
This produces a graph looking like this:

![image](https://github.com/user-attachments/assets/7dcbcf32-59c6-437e-98fc-963e66777697)

From this we can see the clear stages in growth in E.coli. At early points, we see an almost exponentail growth which then plateaus off. Performing a semi-log plot of the same adaty shows that these early stages can be shwon as an increasing linear relationship, which we can then test to obtain estimates for our key values. 

To perform a logistical growth test agaisnt our data, we can use the following code below:

``` {r}
#Case 1. K >> N0, t is small

data_subset1 <- growth_data %>% filter(t<1750) %>% mutate(N_log = log(N))

model1 <- lm(N_log ~ t, data_subset1)
summary(model1)
```
This code above performs a logistical function of our early time points, shown through the *filter(t<1750)*. This is where we expect to observe the bacteria to show logistical growth. From this code, we obtain estimates for both our N0 and our r value. Our N0 is the intial popualtion size and our r value is our growth rate. According to our linear model, the values for N0 are **exp(6.95)** as this is log transfromed data and 0.0099 which can be rounded to **0.01**. To obtaion an estimate for out carrying capacity K, we can perform a logistical analysis without log transforming our data. This is because as t tends towards infintiy, it becomes equal to K. The code to perform this is shown below:

``` {r}
#Case 2. N(t) = K

data_subset2 <- growth_data %>% filter(t>2500)

model2 <- lm(N ~ 1, data_subset2)
summary(model2)
```
The output from this gives us a value of **6.00 x 10<sup>10</sup>**. It is key that our p values for all of these estimates are at the 0.001 level. we can check the appropiateness of this analysis by doing a residuals plot fopr both of these subsest of the data. This can be done from the code below:

``` {r}
residuals_plot <- ggplot(data_subset1, aes(x = t, y = residuals(model1))) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals Plot", x = "t", y = "Residuals")

print(residuals_plot)
```
This produces a plot for when t is small which looks as follows:

![image](https://github.com/user-attachments/assets/11451491-586d-46db-a5e4-3053f11aba03)

``` {r}
residuals_plot2 <- ggplot(data_subset2, aes(x = t, y = residuals(model2))) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals Plot", x = "t", y = "Residuals")

print(residuals_plot2)
```
This produces a residual plot for when N(t) = K which looks as follows:

![image](https://github.com/user-attachments/assets/7bc9a6b6-7463-44c3-b57e-df73894425a9)

We can see in both these plots that the residuals are realtovely close and consistent to 0 in the time stages we are interested in.

Now that we have these estimates, we can plot them against our graph to see how accurate they are. This can be performed using the code below:

``` {r}
N0 <- exp(6.951506) # This is our initial population size of E.coli

r <- 0.01 # t approximation using a linear model 0.009902 rounded to 0.01 

K <- 6.00e+10 # carrying capacity of E.coli

logistic_function <- function(t) {
  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  
  return(N)
  
}

# Plotting the model agaiant our actual data

ggplot(aes(t,N), data = growth_data) +
  
  geom_function(fun=logistic_fun, colour="red") +
  
  geom_point()
```
The graph produced from this shows the real data points in black compared to the model which is shown through the red line. As we can see, this model fits very well against our data points and is very effective in estimating our key parameters for modellling bacterial growth, specifically in E.coli.

![image](https://github.com/user-attachments/assets/2a7a53a8-b867-4e9b-8fef-60ea581b212f)

## Q2) Use your estimates of N0 and r to calculate the population size at t = 4980 min, assuming that the population grows exponentially. How does it compare to the population size predicted under logistic growth?

There are many changes in the assumptions we make when performingn a logistical model comapred to a exponential model. For example, a logistical model assumes that at a certain time point, a carrying capacity is reached and that regardless of any future time from this point, the bacterial population cannot grow due to the factors such as the food availability and available space. This is shown graphically as a plateau. In exponential growth, this consideration is not made, and assumes the bacteria can grow infintely with infinite resource availability, which can commonly lead to unsuitably large estimates in population sizes. 

### Logistical Growth

To see what the popualtion size is at t = 4980 mins for logisitcal growth, we can use the following code:

``` {r}
# Parameters 
N0 <- exp(6.951506) # Initial population size
r <- 0.01           # Growth rate
t <- 4980           # Time in minutes
K <- 6.00e+10       # Carrying capacity (only used for logistical growth)
```
Then we state our logistical equation and include our values above to otain an estimate for N.

``` {r}
logistic_function <- function(t) {
  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  
  return(N)
  
}

#Stating what we want to estimate from t which is 4980
logistic_4980 <- logistic_function(t)

#Printing the estimate
logistic_4980
```
This unsurprisingly gives us a vlaue of **6.00 x 10<sup>10</sup>** which is our carrying capacity. This can also be deduced using the first graph we plottede above showing the data.

### Exponential Growth

To see our estimate for exponential growth, a similar code is used which is shown below, with the same parameters used as show above:

``` {r}
# Exponential growth function
exponential_function <- function(t, N0, r) {
  N <- N0 * exp(r * t)
  return(N)
}

# Obtaining our estimate
exponential_4980 <- exponential_function(t, N0, r)

#Printing the result
exponential_4980
```
This gives us a value of 4.434717 x 10<sup>24</sup> or **4,434,717,000,000,000,000,000,000** which is an immensly larger estimate to what we saw with logistical growth, with a fold difference of 7.39 x 10<sup>13</sup>.

