IF OBJECT_ID('tempdb..#cohort', 'U') IS NOT NULL
	DROP TABLE #cohort;

--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT *
INTO #cohort
FROM 
{@first_occurrence_only} ? {(
	SELECT subject_id,
		MIN(cohort_start_date) AS cohort_start_date,
		MIN(cohort_end_date) AS cohort_end_date
	FROM @cohort_database_schema.@cohort_table
	WHERE cohort_definition_id = @cohort_id
	GROUP BY subject_id
	) c} : {(
	SELECT *
	FROM @cohort_database_schema.@cohort_table
	WHERE cohort_definition_id = @cohort_id 
	) c
};


IF OBJECT_ID('tempdb..#numerator', 'U') IS NOT NULL
	DROP TABLE #numerator;
	
--HINT DISTRIBUTE_ON_KEY(calendar_date)
SELECT calendar_date,
	FLOOR((YEAR(cohort.cohort_start_date) - p.year_of_birth) / 10) age_group,
	gender_concept_id,
	COUNT(DISTINCT subject_id) AS numerator_count
INTO #numerator
FROM #cohort cohort
INNER JOIN @cdm_database_schema.person p ON cohort.subject_id = p.person_id
INNER JOIN @cdm_database_schema.observation_period op ON op.person_id = cohort.subject_id
	AND DATEADD(DAY, @washout_period, op.observation_period_start_date) <= cohort.cohort_start_date
	AND op.observation_period_end_date >= cohort.cohort_start_date 
{@rateType == 'incidence' } ? {
INNER JOIN #calendar_dates cp ON cohort.cohort_start_date = cp.calendar_date } 
{@rateType == 'prevalence' } ? {
INNER JOIN #calendar_dates cp ON cohort.cohort_end_date >= cp.calendar_date
	AND cohort.cohort_start_date <= cp.calendar_date }
GROUP BY calendar_date,
	FLOOR((YEAR(cohort.cohort_start_date) - p.year_of_birth) / 10),
	gender_concept_id;
	
	

IF OBJECT_ID('tempdb..#denominator', 'U') IS NOT NULL
	DROP TABLE #denominator;

--HINT DISTRIBUTE_ON_KEY(calendar_date)
SELECT cp.calendar_date,
	FLOOR((YEAR(cp.calendar_date) - p.year_of_birth) / 10) AS age_group,
	p.gender_concept_id,
	COUNT(DISTINCT p.person_id) AS denominator_count
INTO #denominator
FROM @cdm_database_schema.observation_period op
INNER JOIN @cdm_database_schema.person p ON op.person_id = p.person_id
INNER JOIN (
	SELECT MIN(cohort_start_date) cohort_start_date_min,
		MAX(cohort_end_date) cohort_end_date_max
	FROM #cohort
	) c ON DATEADD(DAY, @washout_period, observation_period_start_date) <= cohort_end_date_max
	AND observation_period_end_date >= cohort_start_date_min
INNER JOIN #calendar_dates cp ON observation_period_start_date <= cp.calendar_date
	AND op.observation_period_end_date >= cp.calendar_date {@first_occurrence_only} ? {
LEFT JOIN #cohort ON subject_id = person_id
	AND cohort_start_date < calendar_date }
WHERE DATEADD(DAY, @washout_period, observation_period_start_date) < observation_period_end_date
	AND DATEADD(DAY, @washout_period, observation_period_start_date) <= cohort_end_date_max
	AND observation_period_end_date >= cohort_start_date_min {@first_occurrence_only} ? {
	AND cohort_start_date IS NULL }
GROUP BY calendar_date,
	FLOOR((YEAR(calendar_date) - year_of_birth) / 10),
	gender_concept_id;
	

IF OBJECT_ID('tempdb..#rates_summary', 'U') IS NOT NULL
	DROP TABLE #rates_summary;

SELECT denominator.calendar_date,
	denominator.age_group,
	concept_name AS gender,
	CASE 
		WHEN numerator.numerator_count IS NOT NULL
			THEN numerator.numerator_count
		ELSE CAST(0 AS INT)
		END AS numerator_count,
	denominator_count
INTO #rates_summary
FROM #denominator denominator
INNER JOIN @cdm_database_schema.concept ON denominator.gender_concept_id = concept_id
LEFT JOIN #numerator numerator ON denominator.calendar_date = numerator.calendar_date
	AND denominator.age_group = numerator.age_group
	AND denominator.gender_concept_id = numerator.gender_concept_id
where denominator_count > 0;

TRUNCATE TABLE #calendar_dates;

DROP TABLE #calendar_dates;

TRUNCATE TABLE #cohort;

DROP TABLE #cohort;

TRUNCATE TABLE #numerator;

DROP TABLE #numerator;

TRUNCATE TABLE #denominator;

DROP TABLE #denominator;
