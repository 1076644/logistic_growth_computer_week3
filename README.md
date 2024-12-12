## Q1) Annotate the README.md file in your logistic_growth repo with more detailed information about the analysis. Add a section on the results and include the estimates for N0, r and K (mention which *.csv file you used).

The data from the file 'experiments.csv' shows data obtained from test tube measurements of a test tube with 900 μl of growth media, and an isolate of the bacterium Escherichia coli suspended in 100 μl of the same media. Logisitcal growth in bacteria can be desrcibed by the following equation:

```math
\begin{equation}
\frac{dN}{dt} = N r (1 - \frac{N}{K})
\end{equation}
```

This represents the rate of change we may see in population growth of E.coli which is then eventually scaled when the population reaches a certain number in the population known as the carrying capacity (K). 

The first section of the analysis is splitting the growth of the bacteria into its distinct stages. In a logisitical model, we see a *lag phase*, *exponential phase* and then a *stationary phase* specifically for this data set. We may also see a *death phase* where population numbers reduce in other models. We can visualise the growth using the following code:

``` r
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

``` r
#Case 1; K >> N0, t is small (Early Growth)

data_subset1 <- growth_data %>% filter(t<1750) %>% mutate(N_log = log(N))

model1 <- lm(N_log ~ t, data_subset1)
summary(model1)
```
This code above performs a logistical function of our early time points, shown through the *filter(t<1750)*. This is where we expect to observe the bacteria to show logistical growth. From this code, we obtain estimates for both our N0 and our r value. Our N0 is the intial popualtion size and our r value is our growth rate. According to our linear model, the values for N0 and r are as follows:
- N0 -> **exp(6.95)** as this is log transfromed data whihc is approximately **1041**
- r -> 0.0099 which can be rounded to **0.01**

To obtain an estimate for our carrying capacity K, we can perform a logistical analysis without log transforming our data. This is because as t tends towards infintiy, it becomes equal to K. The code to perform this is shown below:

``` r
#Case 2: N(t) = K (Stabilising Growth)

data_subset2 <- growth_data %>% filter(t>2500)

model2 <- lm(N ~ 1, data_subset2)
summary(model2)
```
- The output from this gives us a value of **6.00 x 10<sup>10</sup>** for K.

It is key that our p values for all of these estimates are at the 0.001 level. We can check the appropriateness of this analysis by doing a residuals plot for both of these subsets of the data. This can be done from the code below:

``` r
residuals_plot <- ggplot(data_subset1, aes(x = t, y = residuals(model1))) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals Plot", x = "t", y = "Residuals")

print(residuals_plot)
```
This produces a plot for when t is small which looks as follows:

![image](https://github.com/user-attachments/assets/11451491-586d-46db-a5e4-3053f11aba03)

``` r
residuals_plot2 <- ggplot(data_subset2, aes(x = t, y = residuals(model2))) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals Plot", x = "t", y = "Residuals")

print(residuals_plot2)
```
This produces a residual plot for when N(t) = K which looks as follows:

![image](https://github.com/user-attachments/assets/7bc9a6b6-7463-44c3-b57e-df73894425a9)

We can see in both these plots that the residuals are relatively close and consistent to 0 in the time stages we are interested in.

Now that we have these estimates, we can plot them against our graph to see how accurate they are. This can be performed using the code below:

``` r
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
The graph produced from this shows the real data points in black compared to the model which is shown through the red line. As we can see, this model fits very well against our data points and is very effective in estimating our key parameters for modelling bacterial growth, specifically in this E.coli population.

![image](https://github.com/user-attachments/assets/2a7a53a8-b867-4e9b-8fef-60ea581b212f)

## Q2) Use your estimates of N0 and r to calculate the population size at t = 4980 min, assuming that the population grows exponentially. How does it compare to the population size predicted under logistic growth?

There are many changes in the assumptions we make when performing a logistical model compared to a exponential model. For example, a logistical model assumes that at a certain time point, a carrying capacity is reached and that regardless of any future time from this point, the bacterial population cannot exceed this due to factors such as the food availability and available space. This is shown graphically as a plateau. In exponential growth, this consideration is not made, and assumes the bacteria can grow infintely with infinite resource availability, which can commonly lead to unsuitably large estimates in population sizes. 

### Logistical Growth

To see what the population size is at t = 4980 mins for logisitcal growth, we can use the following code:

``` r
# Parameters 
N0 <- exp(6.951506) # Initial population size
r <- 0.01           # Growth rate
t <- 4980           # Time in minutes
K <- 6.00e+10       # Carrying capacity (only used for logistical growth)
```
Then we state our logistical equation and include our values above to obtain an estimate for N.

``` r
logistic_function <- function(t) {
  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  
  return(N)
  
}

#Stating what we want to estimate from t which is 4980
logistic_4980 <- logistic_function(t)

#Printing the estimate
logistic_4980
```
This unsurprisingly gives us a value of **6.00 x 10<sup>10</sup>** which is our carrying capacity. This can also be deduced using the first graph we plotted above showing the data.

### Exponential Growth

To see our estimate for exponential growth, a similar code is used which is shown below, with the same parameters used as shown above:

``` r
# Exponential growth function
exponential_function <- function(t, N0, r) {
  N <- N0 * exp(r * t)
  return(N)
}

# Obtaining our estimate
exponential_4980 <- exponential_function(t, N0, r)

# Printing the result
exponential_4980
```
This gives us a value of 4.434717 x 10<sup>24</sup> or **4,434,717,000,000,000,000,000,000** which is an immensly larger estimate to what we saw with logistical growth, with a fold difference of 7.39 x 10<sup>13</sup>. From this information, we see that in the exponential model, we reach much larger population numbers of E.coli, which seem improbable in the real life conditions of the test tube experiments set up from the data. 

## Q3) Add an R script to your repository that makes a graph comparing the exponential and logistic growth curves (using the same parameter estimates you found). Upload this graph to your repo and include it in the README.md file so it can be viewed in the repo homepage.

### R code

To take the estimates on logistical growth and exponential growth further, we can plot them on the same graph and see the differences in population sizes at both low values of t and high values of t. This allows us to visualise the scaling achieved when a carrying capacity is included in the model. 

The code to perfrom this is shown below:

``` r
growth_data <- growth_data %>%
  mutate(
    Exp_Prediction = exponential_growth(t, N0, r),  # Exponential growth
    Log_Prediction = logistic_growth(t, N0, r, K)  # Logistic growth
  )
```
This line of code produces a prediction for our values for both a logistical model and a exponential model based on the parameters estimated above in our earlier analysis for N0, r and K, which is only used for the logsitical model agaisnt time. We can then take this further by using the ggplot function to visualise the data predictions from these estimated parameters. 

``` r
ggplot(growth_data) +
  # Plot Exponential growth
  geom_line(aes(x = t, y = Exp_Prediction, color = "Exponential Growth", linetype = "Exponential Growth"), 
            linewidth = 1.2) +
  # Plot Logistical growth 
  geom_line(aes(x = t, y = Log_Prediction, color = "Logistic Growth", linetype = "Logistic Growth"), 
            linewidth = 1.2) +

# Add labels and title
  labs(
    title = "Comparison of Exponential and Logistical Growth Models",
    x = "Time (minutes)",
    y = "Population Size (N)",
    color = "Growth Model",  # Label for the legend to distinguish between logistical growth and exponential growth
    linetype = "Growth Model" 
  ) +
  theme_minimal(base_size = 15) + # Theme used for clarity
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    legend.title = element_blank()  # Remove legend title
  ) +
  scale_color_manual(values = c("Exponential Growth" = "red", "Logistic Growth" = "blue")) +
  scale_linetype_manual(values = c("Exponential Growth" = "solid", "Logistic Growth" = "solid")) +
  scale_y_log10()  # Logarithmic scale for population size to better visualize large ranges as t and N appraoch very large values 
```
### Graphical Representation

The code from this produces a graph shown below:

![image](https://github.com/user-attachments/assets/e7bd544f-ecd8-4ff5-87c8-55bd0653c247)

From this graph, it is clear to see that when t is less than 1750, both the logisitical model and and the exponential model follow the same trend in population growth. From this, the logistical growth model plateaus off representing the estimated carrying capacity of that population of E.coli (K), compared to exponential growth which continuosuly increases at the same rate regardless of the value of t. 
