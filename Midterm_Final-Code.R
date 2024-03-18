install.packages("dplyr")
install.packages("moments")
install.packages("psych")
install.packages("car")
install.packages("lmtest")
install.packages("plm")
install.packages(c("ggplot2", "reshape2"))
install.packages("pheatmap")
install.packages("ggthemes")
install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(pheatmap)
library(ggplot2)
library(reshape2)
require(lmtest)
require(plm)
library(car)
library(psych)
library(moments)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(splines)
library(boot)
library(stargazer)

######################
### Import Dataset ###
######################

imdb = read.csv("C:/Users/julie/Downloads/imdb_data_Fall_2023.csv")
attach(imdb)

#View Dataset
View(imdb)

###################################################
### Step 1 - Explore Dataset and pre-processing ###
###################################################

#######################
### Colour Variable ###
#######################

#How many values are there in the colour film column
value_counts= table(imdb$colour_film)
num_unique_values=length(value_counts)
print(num_unique_values) #there are only 2 observations, we can make it binary 

#Transform colour film into binary variable 
imdb=imdb %>%
  mutate(is_color = ifelse(colour_film == "Color", 1, 0))

#Drop initial color variable 
imdb=subset(imdb, select = -colour_film)

#####################
###Genres Variable###
#####################

# Split the "genre" column by "|"
genre_list <- strsplit(imdb$genres, "\\|")

# Combine all genres into a single vector
all_genres <- unlist(genre_list)

# View unique genres
unique_genres <- unique(all_genres)
print(unique_genres)

#See all the observations that contain music 
music_observations = subset(imdb, grepl("\\bMusic\\b", genres, ignore.case = TRUE))

#Create Binary variables for the genres that are missing

#Biography
imdb <- imdb %>%
  mutate(Biography = ifelse(grepl("\\bBiography\\b", genres, ignore.case = TRUE), 1, 0))


imdb <- imdb %>%
  mutate(fantasy = ifelse(grepl("\\bFantasy\\b", genres, ignore.case = TRUE), 1, 0),
         family = ifelse(grepl("\\bFamily\\b", genres, ignore.case = TRUE), 1, 0),
         animation = ifelse(grepl("\\bAnimation\\b", genres, ignore.case = TRUE), 1, 0),
         documentary = ifelse(grepl("\\bDocumentary\\b", genres, ignore.case = TRUE), 1, 0),
         mystery = ifelse(grepl("\\bMystery\\b", genres, ignore.case = TRUE), 1, 0),
         comedy = ifelse(grepl("\\bComedy\\b", genres, ignore.case = TRUE), 1, 0))

#Merge Music into Musical
imdb <- imdb %>%
  mutate(musical = ifelse(grepl("\\bMusic\\b", genres, ignore.case = TRUE) | musical == 1, 1, 0))

#Drop genre column
imdb=subset(imdb, select = -genres)

#######################
###Language Variable###
#######################

#How many values are there in the language column
value_counts= table(imdb$language)
num_unique_values=length(value_counts)
print(num_unique_values) #there are only 19 observations

# Get the unique language values
unique_languages <- unique(imdb$language)

# Print the unique language values
print(unique_languages)

# Use the 'table' function to count the occurrences of each language
language_counts <- table(imdb$language) #There are 1892 observations that are English, also the prediction focuses only on English

# View the count of each language
print(language_counts)

#Let's drop the language variable since there is only english in the test sample
imdb=subset(imdb, select = -language)

########################
####Country Variable####
########################

#How many values are there in the country film column
value_counts= table(imdb$country)
num_unique_values=length(value_counts)
print(num_unique_values) # 34 different values

# Get the unique country values
unique_country <- unique(imdb$country)

# Print the unique country values
print(unique_country)

# Use the 'table' function to count the occurrences of each language
country_counts <- table(imdb$country) #There are 1555 observations that are in USA and 177 in the UK

# View the count of each language
print(country_counts)

# Using subset to filter observations for the USA or the UK
imdb <- subset(imdb, country %in% c("USA", "UK"))

# Create a binary variable 'is_USA'
imdb$is_USA <- ifelse(imdb$country == "USA", 1, 0)

#Let's drop the initial country variable 
imdb=subset(imdb, select = -country)

##########################
####IMDB Link Variable####
##########################

#Let's drop the IMDB link variable since it's useless
imdb=subset(imdb, select = -imdb_link)

############################
####Distributor Variable####
############################

#How many values are there in the cinematographer column
value_counts= table(imdb$distributor)
num_unique_values=length(value_counts)
print(num_unique_values) # 294 different values 

# Get the unique cinematographer values
unique_dist<- unique(imdb$distributor)

# Print the unique cinematographer values
print(unique_dist) # We don't touch this column

# Use the 'table' function to count the occurrences of each production company
dist_counts <- table(imdb$distributor) 

# View the count of each production company
print(dist_counts)

################################
####Cinematographer Variable####
################################

#How many values are there in the cinematographer column
value_counts= table(imdb$cinematographer)
num_unique_values=length(value_counts)
print(num_unique_values) # 660 different values

# Get the unique cinematographer values
unique_cinematographer <- unique(imdb$cinematographer)

# Print the unique cinematographer values
print(unique_cinematographer) # We don't touch this column

# Use the 'table' function to count the occurrences of each production company
cine_counts <- table(imdb$cinematographer) 

# View the count of each production company
print(cine_counts)

###################################
####Production Company Variable####
###################################

#How many values are there in the production company column
value_counts2= table(imdb$production_company)
num_unique_values=length(value_counts2)
print(num_unique_values) # 660 different values

# Get the unique production_company values
unique_production <- unique(imdb$production_company)

# Print the unique production_company values
print(unique_production) # We don't touch this column

# Use the 'table' function to count the occurrences of each production company
production_counts <- table(imdb$production_company) 

# View the count of each production company
print(production_counts)

######################
####Actor Variable####
######################

#How many values are there in the actor column
value_counts= table(imdb$actor1)
num_unique_values=length(value_counts)
print(num_unique_values) # 858 different values

# Get the unique actor values
unique_actor <- unique(imdb$actor1)

# Print the unique actor values
print(unique_actor) # We don't touch this column

# Use the 'table' function to count the occurrences of each production company
actor_counts <- table(imdb$actor1) 

# View the count of each production company
print(actor_counts)

################################
####Maturity Rating Variable####
################################

#How many values are there in the maturity rating column
value_counts= table(imdb$maturity_rating)
num_unique_values=length(value_counts)
print(num_unique_values) # 12 different values

# Get the unique maturity rating values
unique_maturity <- unique(imdb$maturity_rating)

# Print the unique maturity rating values
print(unique_maturity) 

# Use mutate and case_when from dplyr to transform the values

imdb <- imdb %>%
  mutate(maturity_rating = case_when(
    maturity_rating == "Approved" ~ "G",
    maturity_rating == "Passed" ~ "G",
    maturity_rating == "TV-G" ~ "G",
    maturity_rating == "NC-17" ~ "R",
    maturity_rating == "TV-14" ~ "PG-13",
    maturity_rating == "X" ~ "R",
    maturity_rating == "GP" ~ "PG",
    maturity_rating == "M" ~ "R",
    TRUE ~ maturity_rating
  ))

# Create binary variables

imdb <- imdb %>%
  mutate(Rating_R = as.integer(maturity_rating == "R"),
         Rating_PG = as.integer(maturity_rating == "PG"),
         Rating_PG13 = as.integer(maturity_rating == "PG-13"))

#Drop the initial maturity rating column (categorical)
imdb=subset(imdb, select = -maturity_rating)


############################
###Release Month Variable###
############################

# Create a mapping from month abbreviations to numerical values
month_mapping <- c("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6,
                   "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12)

# Use the mapping to convert the 'month' column into numerical format
imdb$release_month= month_mapping[imdb$release_month]


###########################
###Aspect Ratio Variable###
###########################

#How many values are there in the aspect_ratio column
value_counts= table(imdb$aspect_ratio)
num_unique_values=length(value_counts)
print(num_unique_values) # 14 different values

# Get the unique aspect_ratio  values
unique_aspect_ratio <- unique(imdb$aspect_ratio)

# Print the unique maturity rating values
print(unique_aspect_ratio) 

# Use the 'table' function to count the occurrences of each language
aspect_ratio_counts <- table(imdb$aspect_ratio) #There are 784 in 1.85 and 856 in 2.35

# View the count of each language
print(aspect_ratio_counts)

######################################################
### Movie ID, release_day, release_month Variables ###
######################################################

#Drop the Movie ID rating column (label)
imdb=subset(imdb, select = -movie_id)

#Drop the release day column (label)
imdb=subset(imdb, select = -release_day)

#Drop the release month column (label)
imdb=subset(imdb, select = -release_month)

##########################################################################
### distributor, director, actor1, actor2, productioncompany Variables ###
##########################################################################

# Add the frequency column for each variable
imdb$distributor_frequency <- table(imdb$distributor)[imdb$distributor]
imdb$director_frequency <- table(imdb$director)[imdb$director]
imdb$actor1_frequency <- table(imdb$actor1)[imdb$actor1]
imdb$actor2_frequency <- table(imdb$actor2)[imdb$actor2]
imdb$productioncompany_frequency <- table(imdb$production_company)[imdb$production_company]

# Creating Popularity column (0= Not Popular, 1= Not Very Popular, 2= Popular, 3= Very Popular)
imdb$director_popularity <- as.numeric(cut(imdb$director_frequency, breaks=c(0, 2, 4, 6, Inf), labels=c(0, 1, 2, 3)))
imdb$distributor_popularity <- as.numeric(cut(imdb$distributor_frequency, breaks=c(0, 5, 10, 15, Inf), labels=c(0, 1, 2, 3)))
imdb$actor1_popularity <- as.numeric(cut(imdb$actor1_frequency, breaks=c(0, 2, 4, 6, Inf), labels=c(0, 1, 2, 3)))
imdb$actor2_popularity <- as.numeric(cut(imdb$actor2_frequency, breaks=c(0, 2, 4, 6, Inf), labels=c(0, 1, 2, 3)))
imdb$productionCompany_popularity <- as.numeric(cut(imdb$productioncompany_frequency, breaks=c(0, 10, 15, 20, Inf), labels=c(0, 1, 2, 3)))

#Drop the distributor frequency
imdb=subset(imdb, select = -distributor_frequency)

#Drop the director frequency
imdb=subset(imdb, select = -director_frequency)

#Drop the actor 1 frequency
imdb=subset(imdb, select = -actor1_frequency)

#Drop the actor 2 frequency
imdb=subset(imdb, select = -actor2_frequency)

#Drop the productioncompany frequency
imdb=subset(imdb, select = -productioncompany_frequency)

# Attach the dataframe for easier access to its variables
attach(imdb)

#================================================================#
#================================================================#

###############################################
### Step 2 - Explore Variables Individually ###
###############################################

####################################
###Plotting Variable distribution###
####################################

# Function to plot histogram and boxplot for a given variable
plot_variable_distribution <- function(data, var_name) {
  # Histogram
  p1 <- ggplot(data, aes_string(x=var_name)) +
    geom_histogram(fill="blue", alpha=0.7) +
    labs(title=paste("Distribution of", var_name),
         x=var_name,
         y="Number of Movies") +
    theme_minimal()
  
  # Boxplot
  p2 <- ggplot(data, aes_string(y=var_name)) +
    geom_boxplot(fill="blue", alpha=0.7) +
    labs(title=paste("Boxplot of", var_name),
         y=var_name) +
    theme_minimal()
  
  return(list(p1, p2))
}

# Get numeric variables from the dataset
numeric_vars <- names(imdb)[sapply(imdb, is.numeric)]

# Plot all numeric variables
plots <- lapply(numeric_vars, function(var) {
  plot_variable_distribution(imdb, var)
})

# Display the plots
for (plot_pair in plots) {
  grid.arrange(grobs=plot_pair, ncol=2)
}

############################
###Check skewness of data###
############################

# Function to calculate skewness for a variable
calculate_skewness <- function(data, var_name) {
  # Check if the variable is numeric
  if (is.numeric(data[[var_name]])) {
    return(skewness(data[[var_name]], na.rm = TRUE))
  } else {
    return(NA)  # Return NA for non-numeric variables
  }
}

# Get all variables from the dataset
all_vars <- names(imdb)

# Calculate skewness for all variables
skewness_values <- sapply(all_vars, function(var) {
  calculate_skewness(imdb, var)
})

# Display skewness values
skewness_values

#The code calculates and prints the skewness values for each numeric variable in the dataset. Non-numeric variables will have an NA skewness value.

# Calculate and visualize skewness for all variables
for (var in all_vars) {
  skew_value <- calculate_skewness(imdb, var)
  if (!is.na(skew_value)) {
    cat(paste("Skewness for", var, ":", round(skew_value, 2)), "\n")
  }
}

#####################################################
###Run simple linear regression for each predictor###
#####################################################

# Subset only numeric columns for correlation analysis
quantvars <- imdb[sapply(imdb, is.numeric)]

# Compute the correlation matrix
cor_matrix <- cor(quantvars)

# Print the correlation matrix
print(cor_matrix)


# Subset only numeric columns (excluding the IMDb score itself for the regression)
numeric_data <- imdb[sapply(imdb, is.numeric)]
numeric_data$imdb_score <- NULL  

# Create simple linear regressions for each numeric variable against IMDb score
linear_models <- lapply(names(numeric_data), function(var) {
  formula_str <- paste("imdb_score ~", var)
  lm(as.formula(formula_str), data = imdb)
})

# Print summaries of the linear regression models
lapply(linear_models, summary)

####################################
###Drop not significant variables###
####################################

#Drop the aspect ratio
imdb=subset(imdb, select = -aspect_ratio)

#Drop the actor star meter 1
imdb=subset(imdb, select = -actor1_star_meter)

#Drop the actor star meter 2 
imdb=subset(imdb, select = -actor2_star_meter)

#Drop the actor star meter 3
imdb=subset(imdb, select = -actor3_star_meter)

#Drop the adventure genre 
imdb=subset(imdb, select = -adventure)

#Drop the musical genre
imdb=subset(imdb, select = -musical)

#Drop the romance genre
imdb=subset(imdb, select = -romance)

#Drop the animation genre
imdb=subset(imdb, select = -animation)

#Drop the mystery genre
imdb=subset(imdb, select = -mystery)

#Drop the rating pg 
imdb=subset(imdb, select = -Rating_PG)

####################################
###Drop variables not in test data##
####################################

#Drop the scifi genre
imdb=subset(imdb, select = -scifi)

#Drop the western genre
imdb=subset(imdb, select = -western)

# Attach the dataframe for easier access to its variables
attach(imdb)


#================================================================#
#================================================================#

##############################################
###Step 3 - Explore Variables Relationships###
##############################################

##################### 
#######  VIF  #######
#####################

# Fit your linear regression model
lm_model <- lm(imdb_score ~   movie_budget  +  release_year + duration +  nb_news_articles +  nb_faces 
               + action + thriller + sport +  horror +  drama +  war + crime + movie_meter_IMDBpro 
               + is_color + Biography + fantasy +  family +  documentary +  comedy +  is_USA
               + Rating_R + Rating_PG13 + director_popularity + distributor_popularity + actor1_popularity
               + actor2_popularity + productionCompany_popularity, data = imdb)
vif(lm_model)

#####################
#### Correlation ####
#####################

numeric_variables <- imdb[, sapply(imdb, is.numeric)]

# Calculate correlation coefficients
correlations <- sapply(numeric_variables, function(x) cor(x, imdb$imdb_score))
print(correlations)

############################
####Heteroskedasticity######
############################

lm_model <- lm(imdb_score ~   movie_budget  +  release_year + duration +  nb_news_articles +  nb_faces 
               + action + thriller + sport +  horror +  drama +  war + crime + movie_meter_IMDBpro 
               + is_color + Biography + fantasy +  family +  documentary +  comedy +  is_USA
               + Rating_R + Rating_PG13 + director_popularity + distributor_popularity + actor1_popularity
               + actor2_popularity + productionCompany_popularity, data = imdb)

residualPlot(lm_model, quadratic=FALSE)
ncvTest(lm_model)

summary(lm_model)

####################################
####Correct Heteroskedasticity######
####################################

coeftest(lm_model, vcov=vcovHC(lm_model, type="HC1"))

###########################
### Correlation Heat map###
###########################

# Subset only numeric columns for correlation analysis
quantvars <- imdb[sapply(imdb, is.numeric)]

# Compute the correlation matrix
cor_matrix <- cor(quantvars)

# Print the correlation matrix
print(cor_matrix)

# Melt the correlation matrix for use with ggplot2
melted_cor_matrix <- melt(cor_matrix)

# Create a heatmap using ggplot2
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value), color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "", y = "")

#######################################
###Create Studentized Residual Plots###
#######################################

## we used this section for analysis but commented it for conciseness

# Subset only numeric columns (excluding the IMDb score itself for the regression)
#numeric_data <- imdb[sapply(imdb, is.numeric)]
#numeric_data$imdb_score <- NULL  

# Create simple linear regressions and save Q-Q plots of studentized residuals for each numeric variable against IMDb score
#for (var in names(numeric_data)) {
#  formula_str <- paste("imdb_score ~", var)
#  model <- lm(as.formula(formula_str), data = imdb)
#  stud_resids <- rstudent(model)

# Save the Q-Q plots for studentized residuals to a file
#  filename <- paste0("QQ_Plot_", var, ".png")
#  png(filename)
#  qqnorm(stud_resids, main=paste("Q-Q Plot of Studentized Residuals for", var))
#  qqline(stud_resids)
#  dev.off()}

####################
##### Outliers #####
####################

# Subset only numeric columns (excluding the IMDb score itself for the regression)
numeric_data <- imdb[sapply(imdb, is.numeric)]
numeric_data$imdb_score <- NULL  

# Create simple linear regressions and run outlierTest for each numeric variable against IMDb score
outlier_results <- lapply(names(numeric_data), function(var) {
  formula_str <- paste("imdb_score ~", var)
  model <- lm(as.formula(formula_str), data = imdb)
  test_result <- outlierTest(model)
  return(test_result)
})

outlier_results

#########################
####Removing outliers####
#########################

outliers = lm(imdb_score ~   movie_budget  +  release_year + duration +  nb_news_articles +  nb_faces 
              + action + thriller + sport +  horror +  drama +  war + crime + movie_meter_IMDBpro 
              + is_color + Biography + fantasy +  family +  documentary +  comedy +  is_USA
              + Rating_R + Rating_PG13 + director_popularity + distributor_popularity + actor1_popularity
              + actor2_popularity + productionCompany_popularity, data = imdb)
summary(outliers)

imdb1=imdb[-c(1581,316,1806,191,395,492,192),]

no_outliers = lm(imdb_score ~   movie_budget  +  release_year + duration +  nb_news_articles +  nb_faces 
                 + action + thriller + sport +  horror +  drama +  war + crime + movie_meter_IMDBpro 
                 + is_color + Biography + fantasy +  family +  documentary +  comedy +  is_USA
                 + Rating_R + Rating_PG13 + director_popularity + distributor_popularity + actor1_popularity
                 + actor2_popularity + productionCompany_popularity, data = imdb1)
summary(no_outliers)


#================================================================#
#================================================================#

##########################################
################ Step 4 ##################
##########################################

# Try linearity, polynomial and splines for each variable 

variable_list <- names(imdb1)[sapply(imdb1, is.numeric)]
variable_list$imdb_score <- NULL  

# Initialize data frame to store R-squared values and p-values    
r_squared_df <- data.frame(
  Variable = character(),
  Model_Type = character(),
  Degree_or_Knots = integer(),
  R_Squared = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE)

# Test Linearity:
for(var in variable_list) {
  fit_linear <- lm(as.formula(paste("imdb_score ~", var)), data = imdb1)
  coef_summary <- summary(fit_linear)$coefficients
  
  if (nrow(coef_summary) > 1 && "Pr(>|t|)" %in% colnames(coef_summary)) {
    p_val <- coef_summary[2, "Pr(>|t|)"]
    r_squared <- summary(fit_linear)$r.squared
    r_squared_df <- rbind(r_squared_df, data.frame(
      Variable = var,
      Model_Type = "Linear",
      Degree_or_Knots = NA,
      R_Squared = r_squared,
      P_Value = p_val))
  }}


# Test Polynomial
for(var in variable_list) {
  unique_values <- unique(na.omit(imdb[[var]]))
  num_unique_points <- length(unique_values)
  if (num_unique_points > 1) {
    max_degree <- min(6, num_unique_points - 1)
    for(degree in 1:max_degree) {
      fit_poly <- lm(as.formula(paste("imdb_score ~ poly(", var, ", ", degree, ")")), data = imdb1)
      p_val <- summary(fit_poly)$coefficients[2, 4]
      r_squared_df <- rbind(r_squared_df, data.frame(
        Variable = var,
        Model_Type = "Polynomial",
        Degree_or_Knots = degree,
        R_Squared = summary(fit_poly)$r.squared,
        P_Value = p_val))
    }
  }
}

# Test Splines
for(var in variable_list) {
  for(degree in 1:3) {  
    for(knots in c(3, 4, 5)) {  
      fit_spline <- lm(as.formula(paste("imdb_score ~ bs(", var, ", degree=", degree, ", df=", knots, ")")), data = imdb1)
      p_val <- summary(fit_spline)$coefficients[2, 4]
      r_squared_df <- rbind(r_squared_df, data.frame(
        Variable = var,
        Model_Type = "Spline",
        Degree_or_Knots = paste("Degree:", degree, "Knots:", knots),
        R_Squared = summary(fit_spline)$r.squared,
        P_Value = p_val))
    }
  }
}


filtered_df <- subset(r_squared_df, P_Value < 0.05)

options(max.print = 10000)
print(filtered_df)


# Saving it in excel to check the highest R-squares
#write.csv(r_squared_df, "r_squared_summary.csv", row.names = FALSE)

#================================================================#
#================================================================#

##########################################
################ Step 5 ##################
##########################################

attach(imdb1)

#model with duration, nb_news_articles, drama, release_year, and director_popularity: 
model1= "imdb_score ~ movie_meter_IMDBpro + duration + nb_news_articles+ drama + release_year + director_popularity"
mreg1= lm(model1)
summary(mreg1)    ## R-square = 0.2975 ## Adjusted R-square = 0.295
fit = glm(model1)
mse = cv.glm(imdb1, fit, K=40)$delta[1] 
mse  ##MSE = 0.9032

# Trying polynomials on movie_meter_IMDBpro, we got the highest impact with significant result with degree 7
model2= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + release_year + director_popularity"
mreg2= lm(model2)
summary(mreg2)    ## R-square = 0.3891 ## Adjusted R-square = 0.3891
fit = glm(model2)
mse = cv.glm(imdb1, fit, K=40)$delta[1] 
mse  ##MSE = 0.957

# Adding director_popularity to the model:
model3= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + poly(release_year,2) + director_popularity"
mreg3= lm(model3)
summary(mreg3)    ## R-square = 0.3904 ## Adjusted R-square = 0.3857  -> Adjusted R-square increased
fit = glm(model3)
mse = cv.glm(imdb1, fit, K=40)$delta[1] 
mse  ##MSE = 0.846

# Adding actor1_popularity to the model:
model4= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + poly(release_year,2) + director_popularity + actor1_popularity"
mreg4= lm(model4)
summary(mreg4)    ## R-square = 0.3945 ## Adjusted R-square = 0.3896  -> Adjusted R-square increased
fit = glm(model4)
mse = cv.glm(imdb1, fit, K=40)$delta[1] 
mse  ##MSE = 0.8127

# Adding Biography to the model:
model5= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + poly(release_year,2) + director_popularity + actor1_popularity + Biography"
mreg5= lm(model5)
summary(mreg5)    ## R-square = 0.3984 ## Adjusted R-square = 0.3931  -> Adjusted R-square increased
fit = glm(model5)
mse = cv.glm(imdb1, fit, K=40)$delta[1] 
mse  ##MSE = 1.051

# Adding Rating_PG13:
model6= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + poly(release_year,2) + director_popularity + actor1_popularity + Biography + Rating_PG13"
mreg6= lm(model6)
summary(mreg6)    ## R-square = 0.4114 ## Adjusted R-square = 0.4059  -> Adjusted R-square increased
fit = glm(model6)
mse = cv.glm(imdb1, fit, K=40)$delta[1] 
mse  ##MSE = 1.019

# Adding comedy:
model7= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + poly(release_year,2) + director_popularity + actor1_popularity + Biography + Rating_PG13 + comedy"
mreg7= lm(model7)
summary(mreg7)    ## R-square = 0.4114 ## Adjusted R-square = 0.4056  -> Adjusted R-square decreases and not significant -> drop comedy
fit = glm(model7)
mse = cv.glm(imdb1, fit, K=40)$delta[1] 
mse  ##MSE = 1.017

# Adding horror:
model8= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + poly(release_year,2) + director_popularity + actor1_popularity + Biography + Rating_PG13 + horror"
mreg8= lm(model8)
summary(mreg8)    ## R-square = 0.4208 ## Adjusted R-square = 0.4151 -> Adjusted increases -> polynomial for release year becomes not significant so we with it back to linear
fit = glm(model8)
mse = cv.glm(imdb1, fit, K=40)$delta[1] 
mse  ##MSE = 1.188

# Adding is_color:
model9= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + Rating_PG13 + horror + is_color"
mreg9= lm(model9)
summary(mreg9)    ## R-square = 0.4229 ## Adjusted R-square = 0.4172 -> Adjusted increases
fit = glm(model9)
mse = cv.glm(imdb1, fit, K=40)$delta[1] 
mse  ##MSE = 1.184

# adding RatingR:
model10= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + Rating_PG13 + horror + is_color + Rating_R"
mreg10= lm(model10)
summary(mreg10)    ## R-square = 0.4263 ## Adjusted R-square = 0.4203 -> Adjusted increases
fit = glm(model10)
mse = cv.glm(imdb1, fit, K=40)$delta[1] 
mse  ##MSE = 1.04

# Adding action:
model11= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + Rating_PG13 + horror + is_color + Rating_R  + action"
mreg11= lm(model11)
summary(mreg11)    ## R-square = 0.4382 ##Adjusted R-square = 0.4319  -> Adjusted increases
fit = glm(model11)
mse = cv.glm(imdb1, fit, K=40)$delta[1]         ### Rating_PG13 loses significance so we remove it
mse  ##MSE = 0.936

# Removing Rating_PG13:
model11= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action"
mreg11= lm(model11)
summary(mreg11)    ## R-square = 0.4372 ## Adjusted R-square = 0.4313 
fit = glm(model11)
mse = cv.glm(imdb1, fit, K=40)$delta[1]         
mse  ##MSE = 0.82

# Adding is_USA:
model12= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action + is_USA"
mreg12= lm(model12)
summary(mreg12)    ## R-square = 0.4469 ## Adjusted R-square = 0.4408 
fit = glm(model12)
mse = cv.glm(imdb1, fit, K=40)$delta[1]         
mse  ##MSE = 0.7336

# Adding nb_faces:
model13= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action + is_USA + nb_faces"
mreg13= lm(model13)
summary(mreg13)    ## R-square = 0.453 ## Adjusted R-square = 0.4466 
fit = glm(model13)
mse = cv.glm(imdb1, fit, K=40)$delta[1]         
mse  ##MSE = 0.695

# Adding war:
model14= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action + is_USA + nb_faces + war"
mreg14= lm(model14)
summary(mreg14)    ## R-square = 0.4533 ## Adjusted R-square = 0.4466 
fit = glm(model14)
mse = cv.glm(imdb1, fit, K=40)$delta[1]         ### War is not significant so we don't add it
mse  ##MSE = 0.719

# Adding distributor_popularity:
model15= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action + is_USA + nb_faces + distributor_popularity"
mreg15= lm(model15)
summary(mreg15)    ## R-square = 0.4566 ## Adjusted R-square = 0.4499 
fit = glm(model15)
mse = cv.glm(imdb1, fit, K=40)$delta[1]         
mse  ##MSE = 0.716

# Adding movie_budget:
model16= "imdb_score~ poly(movie_meter_IMDBpro,7) + duration + nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action + is_USA + nb_faces + distributor_popularity + movie_budget"
mreg16= lm(model16)
summary(mreg16)    ## R-square = 0.4675 ## Adjusted R-square = 0.4607 
fit = glm(model16)
mse = cv.glm(imdb1, fit, K=40)$delta[1]         
mse  ##MSE = 0.657

# Trying spline for movie_meter_IMDBpro:
model17= "imdb_score~ bs(movie_meter_IMDBpro, degree=1, df=5) + duration + nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action + is_USA + nb_faces + distributor_popularity + movie_budget"
mreg17= lm(model17)
summary(mreg17)    ## R-square = 0.4735 ## Adjusted R-square = 0.4673 
fit = glm(model17)
mse = cv.glm(imdb1, fit, K=40)$delta[1]      ## Distributor_popularity loses significance so we drop it
mse  ##MSE = 0.641

# Adding interaction between duration and nb_news_articles:
model18= "imdb_score~ bs(movie_meter_IMDBpro, degree=1, df=5) + duration * nb_news_articles+ duration + nb_news_articles +drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action + is_USA + nb_faces + movie_budget"
mreg18= lm(model18)
summary(mreg18)    ## R-square = 0.4777 ## Adjusted R-square = 0.4716 
fit = glm(model18)
mse = cv.glm(imdb1, fit, K=40)$delta[1]         
mse  ##MSE = 0.633


###############################################
##### Lets run a Cross Validation Test ########
###############################################

# Run the loop 20 times for both K=10 and K=40
mse_sum_10 = 0
mse_sum_20 = 0
mse_sum_30 = 0
mse_sum_40 = 0
mse_sum_50 = 0

for (i in 1:20) {
  
  # For K=10
  fit_10 <- glm(imdb_score~ bs(movie_meter_IMDBpro, degree=1, df=5) + duration * nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action + is_USA + nb_faces + movie_budget)
  mse_10 <- cv.glm(imdb1, fit_10, K=10)$delta[1]
  mse_sum_10 <- mse_sum_10 + mse_10
  
  # For K=20
  fit_20 <- glm(imdb_score~ bs(movie_meter_IMDBpro, degree=1, df=5) + duration * nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action + is_USA + nb_faces + movie_budget)
  mse_20 <- cv.glm(imdb1, fit_20, K=20)$delta[1]
  mse_sum_20 <- mse_sum_20 + mse_20
  
  # For K=30
  fit_30 <- glm(imdb_score~ bs(movie_meter_IMDBpro, degree=1, df=5) + duration * nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action + is_USA + nb_faces + movie_budget)
  mse_30 <- cv.glm(imdb1, fit_30, K=30)$delta[1]
  mse_sum_30 <- mse_sum_30 + mse_30
  
  # For K=40
  fit_40 <- glm(imdb_score~ bs(movie_meter_IMDBpro, degree=1, df=5) + duration * nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action + is_USA + nb_faces + movie_budget)
  mse_40 <- cv.glm(imdb1, fit_40, K=40)$delta[1]
  mse_sum_40 <- mse_sum_40 + mse_40
  
  # For K=50
  fit_50 <- glm(imdb_score~ bs(movie_meter_IMDBpro, degree=1, df=5) + duration * nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action + is_USA + nb_faces + movie_budget)
  mse_50 <- cv.glm(imdb1, fit_50, K=50)$delta[1]
  mse_sum_50 <- mse_sum_50 + mse_50
}

# Calculate the average MSE for K=10 to K=50
avg_mse_10 = mse_sum_10 / 20
avg_mse_20 = mse_sum_20 / 20
avg_mse_30 = mse_sum_30 / 20
avg_mse_40 = mse_sum_40 / 20
avg_mse_50 = mse_sum_50 / 20

# Compute the overall average of MSEs
avg_mse = (mse_sum_10 + mse_sum_20 + mse_sum_30 + mse_sum_40 + mse_sum_50) / 100

# Print the average MSEs
print(paste("Average MSE with K=10: ", avg_mse_10))
print(paste("Average MSE with K=20: ", avg_mse_20))
print(paste("Average MSE with K=30: ", avg_mse_30))
print(paste("Average MSE with K=40: ", avg_mse_40))
print(paste("Average MSE with K=50: ", avg_mse_50))

print(paste("Average MSE: ", avg_mse))

#================================================================#
#================================================================#

####################################################
####### Final Step : Code for predictions ##########
####################################################

attach(imdb1)
mreg = lm(imdb_score~ bs(movie_meter_IMDBpro, degree=1, df=5) + duration * nb_news_articles+ drama + release_year + director_popularity + actor1_popularity + Biography + horror + is_color + Rating_R  + action + is_USA + nb_faces + movie_budget)
summary(mreg)

## Understanding splines values 
bs_basis <- bs(movie_meter_IMDBpro, degree = 1, df = 5)
head(bs_basis)
dim(bs_basis)
knots_info = attr(mreg$model$`bs(movie_meter_IMDBpro, degree = 1, df = 5)`, "knots")
print(knots_info)

# Generate summary table using stargazer
stargazer(mreg, type = "html", report = "vc*stp")


#################################################
###### Pre-processing on the test data set ######
#################################################

test_data_imdb = read.csv("C:/Users/julie/Downloads/test_data_imdb_Fall_2023.csv")
attach(test_data_imdb)

# Split the "genre" column by "|"
genre_list <- strsplit(test_data_imdb$genres, "\\|")

# Combine all genres into a single vector
all_genres <- unlist(genre_list)

# View unique genres
unique_genres <- unique(all_genres)
print(unique_genres)

#See all the observations that contain music 
music_observations = subset(test_data_imdb, grepl("\\bMusic\\b", genres, ignore.case = TRUE))

#Create Binary variables for the genres that are missing

#Biography
test_data_imdb <- test_data_imdb %>%
  mutate(Biography = ifelse(grepl("\\bBiography\\b", genres, ignore.case = TRUE), 1, 0))


test_data_imdb <- test_data_imdb %>%
  mutate(fantasy = ifelse(grepl("\\bFantasy\\b", genres, ignore.case = TRUE), 1, 0),
         family = ifelse(grepl("\\bFamily\\b", genres, ignore.case = TRUE), 1, 0),
         animation = ifelse(grepl("\\bAnimation\\b", genres, ignore.case = TRUE), 1, 0),
         documentary = ifelse(grepl("\\bDocumentary\\b", genres, ignore.case = TRUE), 1, 0),
         mystery = ifelse(grepl("\\bMystery\\b", genres, ignore.case = TRUE), 1, 0),
         comedy = ifelse(grepl("\\bComedy\\b", genres, ignore.case = TRUE), 1, 0))

#Merge Music into Musical
test_data_imdb <- test_data_imdb %>%
  mutate(musical = ifelse(grepl("\\bMusic\\b", genres, ignore.case = TRUE) | musical == 1, 1, 0))

#Drop genre column
imdb=subset(test_data_imdb, select = -genres)

#Transform colour film into binary variable 
test_data_imdb=test_data_imdb %>%
  mutate(is_color = ifelse(colour_film == "Color", 1, 0))

#Drop initial color variable 
test_data_imdb=subset(test_data_imdb, select = -colour_film)

test_data_imdb <- test_data_imdb %>%
  mutate(Rating_R = as.integer(maturity_rating == "R"),
         Rating_PG = as.integer(maturity_rating == "PG"),
         Rating_PG13 = as.integer(maturity_rating == "PG-13"))

#Drop the initial maturity rating column (categorical)
test_data_imdb=subset(test_data_imdb, select = -maturity_rating)

# Create a binary variable 'is_USA'
test_data_imdb$is_USA <- ifelse(test_data_imdb$country == "USA", 1, 0)

#Let's drop the initial country variable 
test_data_imdb=subset(test_data_imdb, select = -country)

test_data_imdb$movie_budget <- gsub(",", "", test_data_imdb$movie_budget) # Remove commas
test_data_imdb$movie_budget <- as.numeric(test_data_imdb$movie_budget)


# Check the actors and directors of the test dataset popularity
#write.csv(imdb1, "dataset_midterm", row.names = FALSE)

# Manual input of director and actor popularity (based on the conditions stated above when creating the columns)
test_data_imdb$director_popularity <- c(1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2)

test_data_imdb$actor1_popularity <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 4, 1, 1)


#################################
######### PREDICTIONS ###########
#################################

predictions = predict(mreg, test_data_imdb)
print(predictions)

# Predictions along with confidence intervals
predictions_with_ci = predict(mreg, test_data_imdb, interval = "confidence")
print(predictions_with_ci)





#================================================================#
#================================================================#


##########################################
## Some additional plots for the report ##
##########################################

imdb23 = read.csv("C:/Users/julie/Downloads/imdb_data_Fall_2023.csv")
attach(imdb23)


# Histogram with KDE
hist_plot <- ggplot(imdb23, aes(x=imdb_score)) +
  geom_histogram(aes(y=..density..), binwidth=0.37, fill="skyblue", color="black", alpha=0.7) +
  geom_density(color="blue") +
  labs(title="Histogram 1: IMDb Score Distribution", x="IMDb Score", y="Density") +
  theme_minimal()

print(hist_plot)

# Boxplot
box_plot <- ggplot(imdb23, aes(x=imdb_score)) +
  geom_boxplot(fill="lightcoral", color="black", orientation="y") +
  labs(title="Boxplot 1: IMDb Score Distribution", x="IMDb Score", y="") +
  theme_minimal()

print(box_plot)

# Adjust the maturity_rating column
imdb23$maturity_rating <- as.character(imdb23$maturity_rating)
imdb23$maturity_rating[imdb23$maturity_rating %in% c('Approved', 'Passed', 'GP', 'M')] <- 'PG-13'
imdb23$maturity_rating[imdb23$maturity_rating %in% c('X', 'NC-17')] <- 'R'
imdb23$maturity_rating[imdb23$maturity_rating == 'TV-G'] <- 'G'
imdb23$maturity_rating[imdb23$maturity_rating == 'TV-14'] <- 'PG-13'



# Scatter plot for release_year vs. IMDb Score
plot1 <- ggplot(imdb23, aes(x=release_year, y=imdb_score)) +
  geom_point(aes(color=imdb_score), alpha=0.5) +
  ggtitle("IMDb Score vs. Release Year") +
  theme_minimal()

# Scatter plot for movie_budget vs. IMDb Score
plot2 <- ggplot(imdb23, aes(x=movie_budget, y=imdb_score)) +
  geom_point(aes(color=imdb_score), alpha=0.5) +
  ggtitle("IMDb Score vs. Movie Budget") +
  theme_minimal()

# Scatter plot for duration vs. IMDb Score
plot3 <- ggplot(imdb23, aes(x=duration, y=imdb_score)) +
  geom_point(aes(color=imdb_score), alpha=0.5) +
  ggtitle("IMDb Score vs. Duration") +
  theme_minimal()

# Boxplot for IMDb Score by maturity_rating
plot4 <- ggplot(imdb23, aes(x=maturity_rating, y=imdb_score)) +
  geom_boxplot(aes(fill=maturity_rating)) +
  ggtitle("IMDb Score by Maturity Rating") +
  theme_minimal()

# Boxplot for IMDb Score for Drama movies
plot5 <- ggplot(imdb23, aes(x=as.factor(drama), y=imdb_score)) +
  geom_boxplot(aes(fill=as.factor(drama))) +
  ggtitle("IMDb Score for Drama Movies") +
  theme_minimal()

# Boxplot for IMDb Score for Thriller movies
plot6 <- ggplot(imdb23, aes(x=as.factor(thriller), y=imdb_score)) +
  geom_boxplot(aes(fill=as.factor(thriller))) +
  ggtitle("IMDb Score for Thriller Movies") +
  theme_minimal()

# Boxplot for IMDb Score for Romance movies
plot7 <- ggplot(imdb23, aes(x=as.factor(romance), y=imdb_score)) +
  geom_boxplot(aes(fill=as.factor(romance))) +
  ggtitle("IMDb Score for Romance Movies") +
  theme_minimal()

# Scatter plot for movie_meter_IMDBpro vs. IMDb Score
plot8 <- ggplot(imdb23, aes(x=movie_meter_IMDBpro, y=imdb_score)) +
  geom_point(aes(color=imdb_score), alpha=0.5) +
  ggtitle("IMDb Score vs. IMDbPro Movie Meter") +
  theme_minimal()

# Set the size of the plotting window to 8.5 x 11 inches
options(repr.plot.width=8.5, repr.plot.height=11)

# Arrange the plots in a 4x2 grid
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol=2, 
             widths=c(1, 1), heights=c(1, 1, 1, 1))
