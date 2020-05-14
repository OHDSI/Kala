{DEFAULT @washout_period = 365 }
{DEFAULT @first_occurrence_only = TRUE}
{DEFAULT @rateType = 'incidence'}
{DEFAULT @cdm_database_schema = CDM_IBM_CCAE_V1061.dbo}
{DEFAULT @cohort_database_schema = scratch.grao9}
{DEFAULT @cohort_table = epi_727_cohort_CDM_IBM_CCAE_V1061}
{DEFAULT @cohort_id = 440233 }



IF OBJECT_ID('tempdb..#cohort', 'U') IS NOT NULL
	DROP TABLE #cohort;

--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT *
INTO #cohort
FROM
{@first_occurrence_only} ? {
(SELECT subject_id
	,MIN(cohort_start_date) AS cohort_start_date
	,MIN(cohort_end_date) AS cohort_end_date
FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id = @cohort_id
GROUP BY subject_id) c} : {
(SELECT *
FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id = @cohort_id ) c
};


IF OBJECT_ID('tempdb..#cohortEligPersons', 'U') IS NOT NULL
	DROP TABLE #cohortEligPersons;

--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT cohort.subject_id
       , p.gender_concept_id
       , FLOOR((YEAR(cohort.cohort_start_date) - p.year_of_birth) / 10) AS age_group
       , cohort.cohort_start_date
       , cohort.cohort_end_date
       , op.observation_period_start_date
       , op.observation_period_end_date
INTO #cohortEligPersons
FROM #cohort cohort
INNER JOIN @cdm_database_schema.person p ON cohort.subject_id = p.person_id 
INNER JOIN @cdm_database_schema.observation_period op ON op.person_id = cohort.subject_id
	AND DATEADD(DAY, @washout_period, op.observation_period_start_date) <= cohort.cohort_start_date
	AND op.observation_period_end_date >= cohort.cohort_start_date;


IF OBJECT_ID('tempdb..#numerator', 'U') IS NOT NULL
	DROP TABLE #numerator;

SELECT calendar_date,
	age_group,
	gender_concept_id,
	COUNT(distinct subject_id) AS numerator_count
INTO #numerator
FROM #cohortEligPersons cohort
{@rateType == 'incidence'} ? {
INNER JOIN #calendar_dates cp ON cohort.cohort_start_date = cp.calendar_date
}
{@rateType == 'prevalence'} ? {
INNER JOIN #calendar_dates cp ON cohort.cohort_end_date >= cp.calendar_date
	AND cohort.cohort_start_date <= cp.calendar_date
}
GROUP BY calendar_date,
	age_group,
	gender_concept_id;
	
	

IF OBJECT_ID('tempdb..#trunc_op', 'U') IS NOT NULL
	DROP TABLE #trunc_op;

--HINT DISTRIBUTE_ON_KEY(person_id)
SELECT op.person_id,
  	   p.gender_concept_id,
  	   p.year_of_birth,
  		 CASE 
  			WHEN cohort_start_date_min > DATEADD(DAY, @washout_period, observation_period_start_date)
  				THEN cohort_start_date_min
  			ELSE DATEADD(DAY, @washout_period, observation_period_start_date)
  		 END AS observation_period_start_date,
  		 CASE 
  		  WHEN cohort_end_date_max < observation_period_end_date
  				THEN cohort_end_date_max
  			ELSE observation_period_end_date
  		END AS observation_period_end_date
INTO #trunc_op
FROM @cdm_database_schema.observation_period op
INNER JOIN @cdm_database_schema.person p ON op.person_id = p.person_id 
INNER JOIN (
				SELECT MIN(cohort_start_date) cohort_start_date_min,
					   MAX(cohort_end_date) cohort_end_date_max 
				FROM #cohort
			) c ON DATEADD(DAY, @washout_period, observation_period_start_date) <= cohort_end_date_max AND
					observation_period_end_date >= cohort_start_date_min			
WHERE DATEADD(DAY, @washout_period, observation_period_start_date) < observation_period_end_date AND
		  DATEADD(DAY, @washout_period, observation_period_start_date) <= cohort_end_date_max AND
		  observation_period_end_date >= cohort_start_date_min;

		  
IF OBJECT_ID('tempdb..#denominator', 'U') IS NOT NULL
	DROP TABLE #denominator;
	
SELECT DISTINCT cp.calendar_date,
		FLOOR((YEAR(cp.calendar_date) - trunc_op.year_of_birth) / 10) AS age_group,
		trunc_op.gender_concept_id,
		COUNT(distinct trunc_op.person_id) AS denominator_count
INTO #denominator
FROM #trunc_op trunc_op
INNER JOIN #calendar_dates cp ON observation_period_start_date <= cp.calendar_date
	AND trunc_op.observation_period_end_date >= cp.calendar_date
{@first_occurrence_only} ? {
LEFT JOIN #cohort ON subject_id = person_id
	AND cohort_start_date < calendar_date
WHERE cohort_start_date IS NULL
}
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
	AND denominator.gender_concept_id = numerator.gender_concept_id;

TRUNCATE TABLE #calendar_dates;

DROP TABLE #calendar_dates;

TRUNCATE TABLE #cohort;

DROP TABLE #cohort;

TRUNCATE TABLE #cohortEligPersons;

DROP TABLE #cohortEligPersons;

TRUNCATE TABLE #numerator;

DROP TABLE #numerator;

TRUNCATE TABLE #denominator;

DROP TABLE #denominator;
