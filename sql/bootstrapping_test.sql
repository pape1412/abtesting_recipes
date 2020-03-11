--------------------------------------------
/* comparison of means with bootstrapping */
--------------------------------------------
open schema your_schema;

create or replace R set script
	bootstrapping_test(
		data_control double, 	-- Control group data
		data_test double,		-- Test group data
		alpha double,			-- Type I error rate
		resamples int,			-- Bootstrap samples
		ci_type varchar(32)		-- Confidenve interval type: 'percentile' or 't'
)
emits (
	mean_control double,		-- Control group mean
	mean_test double,			-- Test group mean
	diff_means double,			-- Difference in means
	cint_lower double,			-- Lower limit of confidence interval
	cint_upper double,			-- Upper limit of confidence interval
	test_result varchar(256)	-- Test result
) as
run <- function(ctx) {

	# Fetch all records into a single vector
  	ctx$next_row(NA)
	
	# Test statistic for comparison
	diff.means <- function(x,y) mean(x, na.rm=TRUE) - mean(y, na.rm=TRUE)
	
	# Observed difference between groups (H0: There is no difference between Test & Control Groups)
	obs.diff <- diff.means(ctx$data_test, ctx$data_control)
	
	# Bootstrapping test function
	bootstrap.test <- function(x, y, test.statistic, resamples=1000) {
		# Runs two sample bootstrapping test for test statistic of interest
	    #
	    # Args:
	    # - x: Vector with data for either test or control group
	    # - y: Vector with data for either test or control group
	    # - test.statistic: A function which compares groups
	    # - resamples: Number of bootstramp samples to draw (defaults to 1000)
	    #
	    # Returns:
	    # Distribution of test statistic
	    
	    # Size of original samples
	    size.x  <- length(x)
	    size.y <- length(y)
	  
	    # Bootstrapping function
	    bootstrap.samples <- function() {
	        # Sample from test & control with replacement
	        boot.x  <- sample(x, size.x, replace = TRUE)
	        boot.y <- sample(y, size.y, replace = TRUE)
	    
	        # Apply test statistis
	        test.statistic(boot.x, boot.y)
	    }
	    # Draw n bootstrap samples of test & control with replacement (bootstrapping)
	    # and compute test statistic
	    replicate(resamples, bootstrap.samples())
	}
	
	# Bootstrap-t confidence interval function
	bootstrap.t <- function(x, y, test.statistic, alpha = 0.05, resamples = 1000) {
	    # Estimates bootstrap-t confidence intervals for two sample bootstrap test
	    # with test statistic of interest
	    #
	    # Args:
	    # - x: Vector with data for either test or control group
	    # - y: Vector with data for either test or control group
	    # - test.statistic: A function which compares groups
	    # - alpha: Accepted type I error rate (defaults to 0.05)
	    # - resamples: Number of bootstramp samples to draw (defaults to 1000)
	    #
	    # Returns:
	    # intervals: Bootstrap-t confidence intervals for statistic of interest
	    
	    # Define limits
	    ci.lower <- alpha / 2
	    ci.upper <- 1 - (alpha / 2)
	    
	    # Define paramters of original samples
	    size.x <- length(x)
	    size.y <- length(y)
	  
	    obs.diff <- test.statistic(x, y)
	    se <- sqrt((var(x, na.rm = TRUE) / size.x) + (var(y, na.rm = TRUE) / size.y))
	  
	    # Bootstrap resampling function
	    bootstrap.samples <- function() {
	        # Draw n bootstrap samples of test & control with replacement (bootstrapping)
	        boot.x  <- sample(x, size.x, replace = TRUE)
	        boot.y <- sample(y, size.y, replace = TRUE)
	        
	        # Calculate t statistic of bootstrap samples
	        (test.statistic(boot.x, boot.y) - obs.diff) / sqrt((var(boot.x, na.rm = TRUE) / length(boot.x)) + (var(boot.y, na.rm = TRUE) / length(boot.y)))
	    }
	    
	    # Draw n bootstrap samples of test & control with replacement (bootstrapping)
	    # and compute test statistic, the bootstrap t
	    bootstrap.t <- replicate(resamples, bootstrap.samples())
	  
	    # Derive bootstrap-t confidence intervals
	    intervals <- obs.diff - quantile(bootstrap.t, probs = c(ci.upper, ci.lower), na.rm=TRUE) * se
	    names(intervals) <- c(paste0(ci.lower * 100,"%"), paste0(ci.upper * 100, "%"))
	    
	    return(intervals)
	}
	
	# Run bootstrapping test
	# This may take a while to run dependent on the number of resamples
	alpha <- max(ctx$alpha)
	resamples <- max(ctx$resamples)
	bootstrap.results <- bootstrap.test(ctx$data_test, ctx$data_control, diff.means, resamples)
	
	# Compute bootstrap percentile or bootstrap-t confidence intervals
	# for given Type I error rate (alpha)
	if (ctx$ci_type == 'percentile'){
		bootstrap.ci <- quantile(x = bootstrap.results, probs = c(alpha / 2, 1 - (alpha / 2)), na.rm=TRUE)
	} else if (ctx$ci_type == 't'){
		bootstrap.ci <- bootstrap.t(ctx$data_test, ctx$data_control, diff.means, alpha, resamples)
	} else {
		stop('Please specify correct ci type: percentile or t.')
	}
	
	# Evaluate test result
	test_result <- ifelse(
		0 >= bootstrap.ci[1] & 0 <= bootstrap.ci[2],
		paste0("H0 can not be rejected alpha <= ",alpha), 
		paste0("H0 can be rejected at alpha <= ",alpha)
		)
	
	# Return results
  	ctx$emit(
  		mean(ctx$data_control, na.rm=TRUE),
  		mean(ctx$data_test, na.rm=TRUE),
  		obs.diff,
		round(bootstrap.ci[1], 8),
		round(bootstrap.ci[2], 8),
		test_result
  		)
  	}
/


/* create sample data */
create or
replace table
	your_schema.descriptives_example as
select
	user_id,
	case when rand() < 0.5 then 'control' else 'test' end as group_name,
	n
from (
	select
		user_id,
		count(*) as n
	from table.example
	group by 1
)
;


/* run bootstrapping test */
with groups as (
select group_name, n from your_schema.descriptives_example
)
select bootstrapping_test(
	case when group_name = 'control' then n end,
	case when group_name = 'test' then n end,
	0.05,
	1000,
	't'
	)
from groups
;
