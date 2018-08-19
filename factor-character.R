# A random independent variable
continuous_x <- rnorm(10,10,3)

# A random categorical variable as a character vector:
  
character_x  <- (rep(c("dog","cat"),5))

# Convert the character vector to a factor variable. 
factor_x <- as.factor(character_x)

# Give the two categories random values:
  
character_x_value <- ifelse(character_x == "dog", 5*rnorm(1,0,1), rnorm(1,0,2))

# Create a random relationship between the indepdent variables and a dependent variable

continuous_y <- continuous_x*10*rnorm(1,0) + character_x_value

# Compare the output of a linear model with the factor variable and the character vector. Note the warning that is given with the character vector.

summary(lm(continuous_y ~ continuous_x + factor_x))
summary(lm(continuous_y ~ continuous_x + character_x))
