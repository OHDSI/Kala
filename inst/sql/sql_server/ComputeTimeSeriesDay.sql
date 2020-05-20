IF OBJECT_ID('tempdb..#cohort_temp', 'U') IS NOT NULL
	DROP TABLE #cohort_temp;

--HINT DISTRIBUTE_ON_KEY(subject_id)
select *,
		row_number() over(partition by subject_id order by cohort_start_date) as rn
INTO #cohort_temp
FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id = @cohort_id
;


IF OBJECT_ID('tempdb..#cohort', 'U') IS NOT NULL
	DROP TABLE #cohort;

--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT subject_id,
	cohort_start_date,
	cohort_end_date,
	rn
INTO #cohort
FROM #cohort_temp cohort
INNER JOIN @cdm_database_schema.observation_period op
ON cohort.subject_id = op.person_id
AND cohort.cohort_start_date >= DATEADD(DAY, @washout_period, observation_period_start_date)
AND cohort.cohort_start_date <= op.observation_period_end_date
;


IF OBJECT_ID('tempdb..#numerator', 'U') IS NOT NULL
	DROP TABLE #numerator;
	
SELECT cal.calendar_date,
	FLOOR((YEAR(cohort.cohort_start_date) - p.year_of_birth) / 10) age_group,
	p.gender_concept_id,
	/* incidence: calendar_date = cohort_start_date*/
	SUM(CASE 
			WHEN cohort.cohort_start_date = cal.calendar_date THEN 1
			ELSE 0
		 END
		) AS incidence,
	/* prevalence: calendar_date between cohort_start_date and cohort_end_date (both dates inclusive) */
	COUNT(cohort.subject_id) AS prevalence,
	/* incidence: calendar_date = cohort_start_date */
	SUM(CASE 
			WHEN rn = 1 AND cohort.cohort_start_date = cal.calendar_date THEN 1
			ELSE 0
		  END
		) AS incidence_first,
	/* prevalence: calendar_date between cohort_start_date and cohort_end_date (both dates inclusive) */
	SUM(CASE
			WHEN rn = 1 then 1
			ELSE 0
		  END
		) AS prevalence_first
INTO #numerator
FROM #cohort cohort
INNER JOIN @cdm_database_schema.person p ON cohort.subject_id = p.person_id /* n:1 join*/
INNER JOIN #calendar_dates cal ON cal.calendar_date >= cohort.cohort_start_date /* m:n join */
	AND cal.calendar_date <= cohort.cohort_end_date /* many persons have cohort_start_date and cohort_end_dates*/
GROUP BY cal.calendar_date,
	FLOOR((YEAR(cohort.cohort_start_date) - p.year_of_birth) / 10),
	p.gender_concept_id;
	
	
IF OBJECT_ID('tempdb..#denominator_all', 'U') IS NOT NULL
	DROP TABLE #denominator_all;	

SELECT cal.calendar_date,
	FLOOR((YEAR(cal.calendar_date) - p.year_of_birth) / 10) AS age_group,
	p.gender_concept_id,
	COUNT(op.person_id) in_observation
INTO #denominator_all
FROM @cdm_database_schema.observation_period op
INNER JOIN @cdm_database_schema.person p 
ON op.person_id = p.person_id
INNER JOIN #calendar_dates cal 
ON cal.calendar_date >= DATEADD(DAY, @washout_period, op.observation_period_start_date)
	AND cal.calendar_date <= op.observation_period_end_date
GROUP BY cal.calendar_date,
	FLOOR((YEAR(cal.calendar_date) - p.year_of_birth) / 10),
	p.gender_concept_id;

	
IF OBJECT_ID('tempdb..#denominator_first', 'U') IS NOT NULL
	DROP TABLE #denominator_first;
	
SELECT cal.calendar_date,
	FLOOR((YEAR(cal.calendar_date) - p.year_of_birth) / 10) AS age_group,
	p.gender_concept_id,
	COUNT(op.person_id) at_risk_first
INTO #denominator_first
FROM @cdm_database_schema.observation_period op
INNER JOIN @cdm_database_schema.person p 
ON op.person_id = p.person_id
INNER JOIN #calendar_dates cal 
ON cal.calendar_date >= DATEADD(DAY, @washout_period, op.observation_period_start_date)
	AND cal.calendar_date <= op.observation_period_end_date
LEFT JOIN (SELECT * FROM #cohort_temp WHERE rn = 1) cohort
ON op.person_id = cohort.subject_id
AND cohort.cohort_start_date < cal.calendar_date
WHERE cohort.subject_id IS NULL
GROUP BY cal.calendar_date,
	FLOOR((YEAR(cal.calendar_date) - p.year_of_birth) / 10),
	p.gender_concept_id;



IF OBJECT_ID('tempdb..#time_series', 'U') IS NOT NULL
	DROP TABLE #time_series;

SELECT denominator_all.calendar_date,
	denominator_all.age_group,
	concept_name AS gender,
	denominator_all.in_observation,
	denominator_first.at_risk_first,
	numerator.incidence,
	numerator.prevalence,
	numerator.incidence_first,
	numerator.prevalence_first,
	-- denominator_all.in_observation - numerator.prevalence as at_risk,
	denominator_all.in_observation - numerator.prevalence + numerator.incidence as at_risk
INTO #time_series
FROM #denominator_all denominator_all
INNER JOIN @cdm_database_schema.concept ON denominator_all.gender_concept_id = concept_id
LEFT JOIN #denominator_first denominator_first ON denominator_all.calendar_date = denominator_first.calendar_date
	AND denominator_all.age_group = denominator_first.age_group
	AND denominator_all.gender_concept_id = denominator_first.gender_concept_id
LEFT JOIN #numerator numerator ON denominator_all.calendar_date = numerator.calendar_date
	AND denominator_all.age_group = numerator.age_group
	AND denominator_all.gender_concept_id = numerator.gender_concept_id;

TRUNCATE TABLE #calendar_dates;

DROP TABLE #calendar_dates;

TRUNCATE TABLE #numerator;

DROP TABLE #numerator;

TRUNCATE TABLE #denominator_all;

DROP TABLE #denominator_all;

TRUNCATE TABLE #denominator_first;

DROP TABLE #denominator_first;
