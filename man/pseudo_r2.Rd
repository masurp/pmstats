% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pseudo_r2.r
\name{pseudo_r2}
\alias{pseudo_r2}
\title{Pseudo R-Square for Logistic Regression Models}
\usage{
pseudo_r2(..., type = c("McFadden", "Nagelkerke", "CoxSnell"), digits = 2)
}
\arguments{
\item{...}{one or several objects of type \code{glm}, \code{polr}, or \code{multinom}.}

\item{type}{character, one or several out of "McFadden", "Nagelkerke", "CoxSnell".}

\item{digits}{how many digits should be printed?}
}
\value{
A table with all Pseudo-R-Squares.
}
\description{
This functions computes several types of Pseudo-R-Squares from one or several logistic regression models obtained from \code{glm}, \code{polr}, or \code{multinom}.
}
\examples{
# Simulate data
set.seed(666)
x1 = rnorm(1000)           # some continuous variables 
x2 = rnorm(1000)
z = 1 + 2*x1 + 3*x2        # linear combination with a bias
pr = 1/(1+exp(-z))         # pass through an inv-logit function
y = rbinom(1000,1,pr)      # bernoulli response variable
d = data.frame(y=y,x1=x1,x2=x2)

# Estimate models
m1 <- glm(y~x1, data=d, family="binomial")
m2 <- glm(y~x1+x2, data=d, family="binomial")

# Run function
pseudo_r2(m1, m2, digits = 3)
}
