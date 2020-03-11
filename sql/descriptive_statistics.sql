----------------------------
/* descriptive statistics */
----------------------------
open schema your_schema;

create or replace R set script
	summary(group_name varchar(256) UTF8, my_var float)
emits (
	group_name varchar(256) UTF8, nrows int, sum float, min float, q1 float,
	median float, mean float, q3 float,  max float, sd float, p001 float,
	p01 float, p10 float, p20 float, p30 float, p40 float, p50 float, 
	p60 float, p70 float, p80 float, p90 float, p99 float, p999 float
) as
run <- function(ctx) {

	# Fetch all records into a single vector
  	ctx$next_row(NA)
  	
	# Get descriptives 
	statistics <- summary(ctx$my_var)
	
	# Return results
  	ctx$emit(
  		ctx$group_name[1],
  		length(ctx$my_var),
  		sum(ctx$my_var),
  		statistics[1],
  		statistics[2],
  		statistics[3],
  		statistics[4],
  		statistics[5],
  		statistics[6],
  		sd(ctx$my_var),
  		quantile(ctx$my_var, c(.001)),
  		quantile(ctx$my_var, c(.01)),
  		quantile(ctx$my_var, c(.10)),
  		quantile(ctx$my_var, c(.20)),
  		quantile(ctx$my_var, c(.30)),
  		quantile(ctx$my_var, c(.40)),
  		quantile(ctx$my_var, c(.50)),
  		quantile(ctx$my_var, c(.60)),
  		quantile(ctx$my_var, c(.70)),
  		quantile(ctx$my_var, c(.80)),
  		quantile(ctx$my_var, c(.90)),
  		quantile(ctx$my_var, c(.99)),
  		quantile(ctx$my_var, c(.999))
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


/* get descriptives for single variables */
select summary('control', n)
from your_schema.descriptives_example
where group_name = 'control'
;


/* get descriptives for groups */
select summary(group_name, n)
from your_schema.descriptives_example
group by group_name order by group_name
;
