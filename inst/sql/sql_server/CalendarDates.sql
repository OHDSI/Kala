SELECT digits.n
INTO #single_digits
FROM (
	SELECT 1 n
	
	UNION ALL
	
	SELECT 2
	
	UNION ALL
	
	SELECT 3
	
	UNION ALL
	
	SELECT 4
	
	UNION ALL
	
	SELECT 5
	
	UNION ALL
	
	SELECT 6
	
	UNION ALL
	
	SELECT 7
	
	UNION ALL
	
	SELECT 8
	
	UNION ALL
	
	SELECT 9
	
	UNION ALL
	
	SELECT 0
	) digits;

SELECT n1.n * 1 + n2.n * 10 + n3.n * 100 + n4.n * 1000 + n5.n * 10000 AS n
INTO #many_digits
FROM #single_digits n1
	,#single_digits n2
	,#single_digits n3
	,#single_digits n4
	,#single_digits n5;

SELECT DATEADD(DAY, n, CAST('@startDate' AS DATE)) AS calendar_date
INTO #calendar_dates
FROM #many_digits
WHERE n < DATEDIFF(DAY, CAST('@startDate' AS DATE), CAST('@endDate' AS DATE));

TRUNCATE TABLE #single_digits;

DROP TABLE #single_digits;

TRUNCATE TABLE #many_digits;

DROP TABLE #many_digits;