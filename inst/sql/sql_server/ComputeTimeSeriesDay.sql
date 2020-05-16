IF OBJECT_ID('tempdb..#cohort_first', 'U') IS NOT NULL
	DROP TABLE #cohort_first;

--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT subject_id,
	MIN(cohort_start_date) AS cohort_start_date,
	MIN(cohort_end_date) AS cohort_end_date
INTO #cohort_first
FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id = @cohort_id
GROUP BY subject_id;


IF OBJECT_ID('tempdb..#numerator', 'U') IS NOT NULL
	DROP TABLE #numerator;
	
--HINT DISTRIBUTE_ON_KEY(calendar_date)
SELECT cal.calendar_date,
	FLOOR((YEAR(cohort.cohort_start_date) - p.year_of_birth) / 10) age_group,
	p.gender_concept_id,
	-- incidence: calendar_date = cohort_start_date
	COUNT(DISTINCT (CASE 
						WHEN cohort.cohort_start_date = cal.calendar_date THEN cohort.subject_id
						ELSE NULL
					END)
		) AS incidence,
	-- prevalence: calendar_date between cohort_start_date and cohort_end_date (both dates inclusive)
	COUNT(DISTINCT cohort.subject_id) AS prevalence,
	-- incidence: calendar_date = cohort_start_date
	COUNT(DISTINCT (CASE 
						WHEN cohort_first.cohort_start_date = cal.calendar_date THEN cohort_first.subject_id
						ELSE NULL
					END)
		) AS incidence_first,
	-- prevalence: calendar_date between cohort_start_date and cohort_end_date (both dates inclusive)
	COUNT(DISTINCT (CASE
						WHEN cal.calendar_date >= cohort_first.cohort_start_date
							AND cal.calendar_date <= cohort_first.cohort_end_date THEN cohort_first.subject_id
						ELSE NULL
					END)
		) AS prevalence_first
INTO #numerator
FROM @cohort_database_schema.@cohort_table cohort
INNER JOIN #cohort_first cohort_first
ON cohort.subject_id = cohort_first.subject_id
INNER JOIN @cdm_database_schema.person p ON cohort.subject_id = p.person_id
INNER JOIN @cdm_database_schema.observation_period op ON op.person_id = cohort.subject_id
	AND DATEADD(DAY, @washout_period, op.observation_period_start_date) <= cohort.cohort_start_date
	AND op.observation_period_end_date >= cohort.cohort_start_date 
INNER JOIN #calendar_dates cal ON cal.calendar_date >= cohort.cohort_start_date
	AND cal.calendar_date <= cohort.cohort_end_date
GROUP BY cal.calendar_date,
	FLOOR((YEAR(cohort.cohort_start_date) - p.year_of_birth) / 10),
	p.gender_concept_id;
	
	

IF OBJECT_ID('tempdb..#denominator', 'U') IS NOT NULL
	DROP TABLE #denominator;

--HINT DISTRIBUTE_ON_KEY(calendar_date)
SELECT cal.calendar_date,
	FLOOR((YEAR(cal.calendar_date) - p.year_of_birth) / 10) AS age_group,
	p.gender_concept_id,
	-- atrisk: these are indviduals who on a particular date are 
	-- either not in the cohort i.e. cohort_start_date IS NULL
	-- or, calendar_date is on or before cohort_start_date
	COUNT(DISTINCT (CASE
						WHEN cohort.cohort_start_date IS NULL THEN op.person_id
						WHEN cal.calendar_date <= cohort.cohort_start_date THEN op.person_id
						ELSE NULL
					END
					)	
	) AS atrisk,
	-- atrisk: these are indviduals who on a particular date are 
	-- either not in the cohort_first i.e. cohort_start_date IS NULL
	-- or, calendar_date is on or before cohort_start_date of the first occurrence cohort
	COUNT(DISTINCT (CASE
						WHEN cohort_first.cohort_start_date IS NULL THEN op.person_id
						WHEN cal.calendar_date >= cohort_first.cohort_start_date AND
							 cal.calendar_date <= cohort_first.cohort_end_date THEN op.person_id
						ELSE NULL
					END
					)	
	) AS atriskFirst,
	COUNT(DISTINCT op.person_id)	in_observation
INTO #denominator
FROM @cdm_database_schema.observation_period op
INNER JOIN @cdm_database_schema.person p ON op.person_id = p.person_id
INNER JOIN (
	SELECT 	MIN(cohort_start_date) cohort_start_date_min,
			MAX(cohort_end_date) cohort_end_date_max
	FROM @cohort_database_schema.@cohort_table
	) c ON DATEADD(DAY, @washout_period, op.observation_period_start_date) <= c.cohort_end_date_max
	AND op.observation_period_end_date >= c.cohort_start_date_min
INNER JOIN #calendar_dates cal ON cal.calendar_date >= op.observation_period_start_date
	AND cal.calendar_date <= op.observation_period_end_date
LEFT JOIN @cohort_database_schema.@cohort_table cohort ON cohort.subject_id = op.person_id
	AND cohort.cohort_start_date < cal.calendar_date
LEFT JOIN #cohort_first cohort_first ON cohort.subject_id = op.person_id
	AND cohort_first.cohort_start_date < cal.calendar_date
WHERE DATEADD(DAY, @washout_period, op.observation_period_start_date) < op.observation_period_end_date
	AND DATEADD(DAY, @washout_period, op.observation_period_start_date) <= cohort_end_date_max
	AND op.observation_period_end_date >= c.cohort_start_date_min
GROUP BY cal.calendar_date,
	FLOOR((YEAR(cal.calendar_date) - p.year_of_birth) / 10),
	p.gender_concept_id;
	

IF OBJECT_ID('tempdb..#rates_summary', 'U') IS NOT NULL
	DROP TABLE #rates_summary;

SELECT denominator.calendar_date,
	denominator.age_group,
	concept_name AS gender,
	numerator.incidence,
	numerator.prevalence,
	numerator.incidence_first,
	numerator.prevalence_first,
	denominator.atrisk,
	denominator.atriskFirst
INTO #rates_summary
FROM #denominator denominator
INNER JOIN @cdm_database_schema.concept ON denominator.gender_concept_id = concept_id
LEFT JOIN #numerator numerator ON denominator.calendar_date = numerator.calendar_date
	AND denominator.age_group = numerator.age_group
	AND denominator.gender_concept_id = numerator.gender_concept_id;

TRUNCATE TABLE #calendar_dates;

DROP TABLE #calendar_dates;

TRUNCATE TABLE #cohort_first;

DROP TABLE #cohort_first;

TRUNCATE TABLE #numerator;

DROP TABLE #numerator;

TRUNCATE TABLE #denominator;

DROP TABLE #denominator;
