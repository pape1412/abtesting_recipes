------------
/* t-test */
------------
open schema your_schema;

create or replace R set script
	t_test(
		data_control float,		-- Control group data
		data_test float			-- Test group data
)
emits (
	mean_test float,			-- Test group mean
	mean_control float,			-- Control group mean
	diff_means float,			-- Difference in means
	statistic float,			-- T statistic
	df int,						-- Degrees of freedom
	pval float,					-- P value
	cint_lower float,			-- Lower limit of confidence interval
	cint_upper float,			-- Upper limit of confidence interval
	test_method varchar(256)	-- Test method
) as
run <- function(ctx) {

	# Fetch all records into a single vector
  	ctx$next_row(NA)
  	
  	# Observed difference in means
	diff.means <- mean(ctx$data_test, na.rm=TRUE) - mean(ctx$data_control, na.rm=TRUE)
  	
	# Run test
	results <- t.test(ctx$data_test, ctx$data_control)
	
	# Return results
  	ctx$emit(
  		results$estimate[1],
  		results$estimate[2],
  		diff.means,
  		results$statistic,
		floor(results$parameter),
		results$p.value,
		results$conf.int[1],
		results$conf.int[2],
		results$method
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


/* runt t-test */
with groups as (
select group_name, n from your_schema.descriptives_example
)
select t_test(
	case when group_name = 'control' then n end,
	case when group_name = 'test' then n end
	)
from groups
;
