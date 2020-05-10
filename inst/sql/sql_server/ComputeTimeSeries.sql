{DEFAULT @washout_period = 365 }
{DEFAULT @first_occurrence_only = TRUE}
{DEFAULT @rateType = 'incidence'}
{DEFAULT @cdm_database_schema = CDM_IBM_CCAE_V1061.dbo}
{DEFAULT @cohort_database_schema = scratch.grao9}
{DEFAULT @cohort_table = epi_727_cohort_CDM_IBM_CCAE_V1061}
{DEFAULT @cohort_id = 440233 }

IF OBJECT_ID('tempdb..#numerator', 'U') IS NOT NULL
	DROP TABLE #numerator;

SELECT period_begin,
	period_end,
	FLOOR((YEAR(cohort_start_date) - year_of_birth) / 10) AS age_group,
	gender_concept_id,
	COUNT(*) AS cohort_count
INTO #numerator
FROM (
	{@first_occurrence_only} ? { SELECT subject_id,
		MIN(cohort_start_date) AS cohort_start_date,
		MIN(cohort_end_date) AS cohort_end_date
	FROM @cohort_database_schema.@cohort_table
	WHERE cohort_definition_id = @cohort_id
	GROUP BY subject_id } : {
	SELECT *
	FROM @cohort_database_schema.@cohort_table
	WHERE cohort_definition_id = @cohort_id }
	) cohort
INNER JOIN @cdm_database_schema.person ON subject_id = person.person_id
INNER JOIN @cdm_database_schema.observation_period ON observation_period.person_id = person.person_id
	AND DATEADD(DAY, @washout_period, observation_period_start_date) <= cohort_start_date
	AND observation_period_end_date >= cohort_start_date
{@rateType == 'incidence'} ? {
INNER JOIN #calendar_periods cp ON cohort.cohort_start_date >= cp.period_begin
	AND cohort.cohort_start_date <= cp.period_end
}
{@rateType == 'prevalence'} ? {
INNER JOIN #calendar_periods cp ON cohort.cohort_end_date >= cp.period_begin
	AND cohort.cohort_start_date <= cp.period_end
}
GROUP BY period_begin,
	period_end,
	FLOOR((YEAR(cohort_start_date) - year_of_birth) / 10),
	gender_concept_id;

IF OBJECT_ID('tempdb..#denominator', 'U') IS NOT NULL
	DROP TABLE #denominator;

SELECT period_begin,
	period_end,
	age_group,
	gender_concept_id,
	SUM(CAST(DATEDIFF(DAY, start_date, end_date) AS BIGINT)) / 365.25 AS person_years
INTO #denominator
FROM (
	{@first_occurrence_only} ? { 
	SELECT period_begin,
		period_end,
		age_group,
		gender_concept_id,
		CASE 
			WHEN cohort_start_date IS NOT NULL
				THEN CASE 
						WHEN cohort_start_date < period_begin
							THEN start_date
						ELSE cohort_start_date
						END
			ELSE start_date
			END AS start_date,
		CASE 
			WHEN cohort_start_date IS NOT NULL
				THEN CASE 
						WHEN cohort_start_date >= period_begin
							THEN cohort_start_date
						ELSE end_date
						END
			ELSE end_date
			END AS end_date
	FROM (
		} SELECT cp.period_begin,
			cp.period_end,
			person.person_id,
			FLOOR((YEAR(period_begin) - year_of_birth) / 10) AS age_group,
			gender_concept_id,
			CASE 
				WHEN observation_period_start_date > period_begin
					THEN observation_period_start_date
				ELSE period_begin
				END AS start_date,
			CASE 
				WHEN observation_period_end_date < period_end
					THEN observation_period_end_date
				ELSE period_end
				END AS end_date
		FROM (
			SELECT person_id,
				DATEADD(DAY, @washout_period, observation_period_start_date) AS observation_period_start_date,
				observation_period_end_date
			FROM @cdm_database_schema.observation_period
			WHERE DATEADD(DAY, @washout_period, observation_period_start_date) < observation_period_end_date
			) trunc_op
		INNER JOIN #calendar_periods cp ON observation_period_start_date <= cp.period_end
			AND observation_period_end_date >= cp.period_begin
		INNER JOIN @cdm_database_schema.person ON trunc_op.person_id = person.person_id 
		{@first_occurrence_only} ? {
		) time_spans_1
	LEFT JOIN (
		SELECT subject_id,
			MIN(cohort_start_date) AS cohort_start_date
		FROM @cohort_database_schema.@cohort_table
		WHERE cohort_definition_id = @cohort_id
		GROUP BY subject_id
		) cohort ON subject_id = person_id
		AND cohort_start_date <= period_end }
	) time_spans_2
GROUP BY period_begin,
	period_end,
	age_group,
	gender_concept_id;

IF OBJECT_ID('tempdb..#rates_summary', 'U') IS NOT NULL
	DROP TABLE #rates_summary;

SELECT denominator.period_begin,
	denominator.period_end,
	denominator.age_group,
	concept_name AS gender,
	CASE 
		WHEN numerator.cohort_count IS NOT NULL
			THEN numerator.cohort_count
		ELSE CAST(0 AS INT)
		END AS cohort_count,
	person_years
INTO #rates_summary
FROM #denominator denominator
INNER JOIN @cdm_database_schema.concept ON denominator.gender_concept_id = concept_id
LEFT JOIN #numerator numerator ON denominator.period_begin = numerator.period_begin
	AND denominator.period_end = numerator.period_end
	AND denominator.age_group = numerator.age_group
	AND denominator.gender_concept_id = numerator.gender_concept_id;

TRUNCATE TABLE #calendar_periods;

DROP TABLE #calendar_periods;

TRUNCATE TABLE #numerator;

DROP TABLE #numerator;

TRUNCATE TABLE #denominator;

DROP TABLE #denominator;
