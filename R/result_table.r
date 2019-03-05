#' Result table for linear models, (bayesian) multilevel models and structural equation models
#' 
#' This function creates a printable results table based objects of class \code{lm}, \code{lmerModLmerTest}, \code{brmsfit} or \code{lavaan}. Several arguments can be specified in order to customize the output. 
#' 
#' @param object An object of class \code{lm}, \code{lmerModLmerTest} or \code{lavaan}. 
#' @param new_labels A character vector with new labels for the paths in the model (needs to have the same number of values as paths in the model, potentially including the intercept). 
#' @param var_label When using lavaan for SEM, paths can be labelled in the model. With this argument, one can specify which of the labelled paths should be printed (takes a single character value or a character ). This argument only works with objects of class \code{lavaan}.
#' @param labels A logical value specifying whether only paths with labels should be included. This argument only works with objects of class \code{lavaan}.
#' @param regressions A logical value specifying whether only regressions should be included. This argument only works with objects of class \code{lavaan}.
#' @param std A logical value indicating whether standarized coefficient should be included (works only with objects of class \code{lm} and \code{lavaan}).
#' @param hdi_prob Vector of scalars between 0 and 1, indicating the mass within the credible interval that is to be estimated (works only with objects of class \code{brmsfit}).
#' @param typical The typical value that will represent the Bayesian point estimate. By default, the posterior median is returned (possible values: "median", "mean", "weighted.mean", or "mode")
#' @param print A logical value indicating whether the resulting table should be formatted according to APA-guidelines.
#' @examples 
#' ## Example 1: Linear model
#' mod.lm <- lm(mpg ~ cyl, mtcars)
#' result_table(mod.lm, new_labels = c("", "H1"), print = TRUE)
#'
#' ## Example 2: Multilevel model
#' mod.lmer <- lmerTest::lmer(Reaction ~ 1 + Days + (1 | Subject), sleepstudy)
#' result_table(mod.lmer)
#' result_table(mod.lmer, new_labels = c(1, 2))
#' 
#' ## Example 3: Structural equation model
#' model.sem <- '
#'   # latent variables
#'   ind60 =~ x1 + x2 + x3
#'   dem60 =~ y1 + y2 + y3 + y4
#'   dem65 =~ y5 + y6 + y7 + y8
#' 
#'   # regressions
#'   dem60 ~ a*ind60
#'   dem65 ~ b*ind60 + c*dem60
#' 
#'   # residual covariances
#'   y1 ~~ y5
#'   y2 ~~ y4 + y6
#'   y3 ~~ y7
#'   y4 ~~ y8
#'   y6 ~~ y8
#' '
#' fit.sem <- sem(model.sem, data = PoliticalDemocracy)
#' result_table(fit.sem, regressions = TRUE)
#' result_table(fit.sem, labels = TRUE)
#' result_table(fit.sem, labels = TRUE, new_labels = c("H1", "H2", "H3"), std = FALSE, print = TRUE)
#' 
#' 
#' # Example 4: Bayesian multilevel modelling
#' ## Probit regression using the binomial family
#' ntrials <- sample(1:10, 100, TRUE)
#' success <- rbinom(100, size = ntrials, prob = 0.4)
#' x <- rnorm(100)
#' data <- data.frame(ntrials, success, x)
#' fit <- brm(success | trials(ntrials) ~ x, data = data,
#'            family = binomial("probit"))
#' result_table(fit, hdi_prob = .95, print = TRUE)
#' @export
result_table <- function(object,
                         new_labels = NULL,
                         var_label = NULL,
                         labels = FALSE,
                         regressions = FALSE,
                         std = TRUE,
                         hdi_prob = .90,
                         typical = "median",
                         print = FALSE){
  
  # dependencies 
  library(tidyverse)
  library(lavaan)
  library(papaja)
  library(magrittr)
  library(QuantPsyc)
  library(sjstats)
  
  if (is.element("lm", class(object))) {
    
    # get coefficients
    coefs <- summary(object)$coeff
    
    # get confidence intervals
    cis   <- confint(object)
    
    # get standardized coefficients (beta)
    beta  <- c(NA, lm.beta(object) %>% as.vector)
    
    # bind together and rename
    temp <- cbind(coefs, cis) %>%
      as.data.frame %>% 
      rownames_to_column("predictor") %>%
      set_colnames(., c("predictor", "b", "se", "t", "p", "ll", "ul")) %>%
      dplyr::select(predictor, b, se, ll, ul, p) %>%
      as.tibble
    
    if (isTRUE(std)) {
      names(beta) <- temp$predictor
      temp <- cbind(temp, beta) %>%
        as.tibble
    }
    
    if (!is.null(new_labels)) {
      temp <- temp %>%
        cbind(., new_labels) %>%
        mutate(label = as.character(new_labels)) %>%
        dplyr::select(predictor, label, everything(), -new_labels) %>%
        as.tibble
    }
    
    # --- extracting results from multilevel models --- #
  } else if (is.element("lmerModLmerTest", class(object))) {
    
    # get coefficients
    coeffs <- summary(object)$coef %>%
      as.data.frame %>%
      rownames_to_column("predictor")
    
    # get confidence intervals
    cis <- confint(object) %>%
      as.tibble %>% 
      slice(3:nrow(.))
    
    # bind table
    temp <- cbind(coeffs, cis) %>%
      as.tibble %>%
      set_colnames(c("predictor", "b", "se", "df", "t", "p", "ll", "ul")) %>%
      dplyr::select(predictor, b, se, ll, ul, p)
    
    if (!is.null(new_labels)) {
      temp <- temp %>%
        cbind(., new_labels) %>%
        mutate(label = as.character(new_labels)) %>%
        dplyr::select(predictor, label, everything(), -new_labels) %>%
        as.tibble
    }
    
    # --- extracting results from SEM --- #
  } else if (is.element("lavaan", class(object))) {
    
    # get Coefficients
    if(!is.null(var_label)){
      coeffs <- object %>%
        parameterEstimates(standardized = TRUE) %>% 
        filter(label %in% var_label)
    } else if (isTRUE(regressions)) {
      coeffs <- object %>%
        parameterEstimates(standardized = TRUE) %>%
        filter(op == "~")
    } else if (isTRUE(labels)) {
      coeffs <- object %>%
        parameterEstimates(standardized = TRUE) %>%
        subset(label != "")
    } else {
      coeffs <- object %>%
        parameterEstimates(standardized = TRUE)
    }
    
    temp <- coeffs %>% 
      dplyr::select(outcome = lhs, 
                    predictor = rhs,
                    b = est,
                    se = se,
                    ll = ci.lower, 
                    ul = ci.upper, 
                    p = pvalue,
                    beta = "std.all") %>%
      as.tibble
    
    if (!isTRUE(std)) {
      temp <- temp %>%
        dplyr::select(outcome, predictor, b, se, ll, ul, p)
    }
    
    if (!is.null(new_labels)) {
      temp <- temp %>%
        cbind(., new_labels) %>%
        mutate(label = as.character(new_labels)) %>%
        dplyr::select(outcome, predictor, label, everything(), -new_labels) %>%
        as.tibble
    }
    
    # --- extracting results from bayesian multilevel models --- #
  } else if (is.element("brmsfit", class(object))) {
    temp <- object %>%
      tidy_stan(., prob = hdi_prob, typical = typical) %>%
      as.tibble %>%
      set_colnames(c("predictor", "median", "se", "ll", "ul", "ratio", "rhat", "mcse"))
    
    if (!is.null(new_labels)) {
      temp <- temp %>%
        cbind(., new_labels) %>%
        mutate(label = as.character(new_labels)) %>%
        dplyr::select(predictor, label, everything(), -new_labels) %>%
        as.tibble
    }
  } 
  
  if (isTRUE(print) & is.element("brmsfit", class(object))) {
    temp <- temp %>%
      mutate_at(vars(median, se, ll, ul, ratio, rhat, mcse), funs(printnum(.)))
  } else if (isTRUE(print)) {
    temp <- temp %>% 
      mutate_at(vars(b, se, ll, ul), funs(printnum(.))) %>% 
      mutate(p = printp(p))
    if ("beta" %in% names(temp)) {
      temp <- temp %>%
        mutate(beta = printnum(beta, gt1 = F))
    }
  }
  
  return(temp)
  
}


