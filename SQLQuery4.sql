/*
--Test Declarations
declare @BegDate datetime
declare @EndDate datetime
declare @Locations varchar (max)
declare @BudgetID char (15)

--Test Criteria
set @begdate = '10/01/2015'
set @enddate = '10/28/2015'
set @locations = '1212'
set @budgetID = 'BUDGET 2015'
--*/


IF object_id('tempdb..#tempTerritoriesAndStores') IS NOT NULL
    DROP TABLE #tempTerritoriesAndStores
IF object_id('tempdb..#tempHeaderAccounts010') IS NOT NULL
    DROP TABLE #tempHeaderAccounts010
IF object_id('tempdb..#tempbudget01') IS NOT NULL
    DROP TABLE #tempbudget01     
IF object_id('tempdb..#tempbudget02') IS NOT NULL
    DROP TABLE #tempbudget02      
IF object_id('tempdb..#tempbudget03') IS NOT NULL
    DROP TABLE #tempbudget03    
IF object_id('tempdb..#tempbudget04') IS NOT NULL
    DROP TABLE #tempbudget04    
IF object_id('tempdb..#tempbudget05') IS NOT NULL
    DROP TABLE #tempbudget05   
IF object_id('tempdb..#tempBudget06') IS NOT NULL
    DROP TABLE #tempBudget06  
IF object_id('tempdb..#tempSalesComparison010') IS NOT NULL
    DROP TABLE #tempSalesComparison010  
IF object_id('tempdb..#tempSalesComparison020') IS NOT NULL
    DROP TABLE #tempSalesComparison020                 
IF object_id('tempdb..#tempUltiproExpenses') IS NOT NULL
    DROP TABLE #tempUltiproExpenses     
IF object_id('tempdb..#tempCPTO') IS NOT NULL
    DROP TABLE #tempCPTO 
IF object_id('tempdb..#payroll_import_calculate') IS NOT NULL
    DROP TABLE #payroll_import_calculate 
IF object_id('tempdb..#tempLaborHoursByLocation010') IS NOT NULL
    DROP TABLE #tempLaborHoursByLocation010     
IF object_id('tempdb..#tempGSG2') IS NOT NULL
    DROP TABLE #tempGSG2
IF object_id('tempdb..#tempGSG') IS NOT NULL
    DROP TABLE #tempGSG     
IF object_id('tempdb..#tempTotalLabor') IS NOT NULL
    DROP TABLE #tempTotalLabor    
IF object_id('tempdb..#tempUltiproExpensesRETRO') IS NOT NULL
    DROP TABLE #tempUltiproExpensesRETRO
    
    
IF object_id('tempdb..#tempUltiproExpensesRETRO') IS NOT NULL
    DROP TABLE #tempUltiproExpensesRETRO
IF object_id('tempdb..#tempUltiproExpensesRETRO_HOLIDAY') IS NOT NULL
    DROP TABLE #tempUltiproExpensesRETRO_HOLIDAY    
IF object_id('tempdb..#tempUltiproExpensesREFFERAL') IS NOT NULL
    DROP TABLE #tempUltiproExpensesREFFERAL
IF object_id('tempdb..#tempUltiproExpensesBONUS') IS NOT NULL
    DROP TABLE #tempUltiproExpensesBONUS   
IF object_id('tempdb..#tempUltiproExpensesANNIVERSITY') IS NOT NULL
    DROP TABLE #tempUltiproExpensesANNIVERSITY 
IF object_id('tempdb..#tempTotaLabor2') IS NOT NULL
    DROP TABLE #tempTotaLabor2 

IF object_id('tempdb..#tempUltiproExpensesRETRO') IS NOT NULL
    DROP TABLE #tempUltiproExpensesRETRO
IF object_id('tempdb..#tempbudget') IS NOT NULL
    DROP TABLE #tempbudget   
IF object_id('tempdb..#tempRevenueByLocation010') IS NOT NULL
    DROP TABLE #tempRevenueByLocation010
IF object_id('tempdb..#tempTotalDonorsByLocation') IS NOT NULL
    DROP TABLE #tempTotalDonorsByLocation  
IF object_id('tempdb..#tempMetrics020') IS NOT NULL
    DROP TABLE #tempMetrics020
IF object_id('tempdb..#tempMetrics010') IS NOT NULL
    DROP TABLE #tempMetrics010

IF object_id('tempdb..#tempTerritoriesAndStores') IS NOT NULL
    DROP TABLE #tempTerritoriesAndStores
IF object_id('tempdb..#tempHeaderAccounts010') IS NOT NULL
    DROP TABLE #tempHeaderAccounts010
IF object_id('tempdb..#tempbudget01') IS NOT NULL
    DROP TABLE #tempbudget01     
IF object_id('tempdb..#tempbudget02') IS NOT NULL
    DROP TABLE #tempbudget02      
IF object_id('tempdb..#tempbudget03') IS NOT NULL
    DROP TABLE #tempbudget03    
IF object_id('tempdb..#tempbudget04') IS NOT NULL
    DROP TABLE #tempbudget04    
IF object_id('tempdb..#tempbudget05') IS NOT NULL
    DROP TABLE #tempbudget05   
IF object_id('tempdb..#tempBudget06') IS NOT NULL
    DROP TABLE #tempBudget06  
IF object_id('tempdb..#tempSalesComparison010') IS NOT NULL
    DROP TABLE #tempSalesComparison010  
IF object_id('tempdb..#tempSalesComparison020') IS NOT NULL
    DROP TABLE #tempSalesComparison020                 
IF object_id('tempdb..#tempUltiproExpenses') IS NOT NULL
    DROP TABLE #tempUltiproExpenses     
IF object_id('tempdb..#tempCPTO') IS NOT NULL
    DROP TABLE #tempCPTO 
IF object_id('tempdb..#payroll_import_calculate') IS NOT NULL
    DROP TABLE #payroll_import_calculate 
IF object_id('tempdb..#tempLaborHoursByLocation010') IS NOT NULL
    DROP TABLE #tempLaborHoursByLocation010     
IF object_id('tempdb..#tempGSG2') IS NOT NULL
    DROP TABLE #tempGSG2
IF object_id('tempdb..#tempGSG') IS NOT NULL
    DROP TABLE #tempGSG     
IF object_id('tempdb..#tempTotalLabor') IS NOT NULL
    DROP TABLE #tempTotalLabor    
IF object_id('tempdb..#tempUltiproExpensesRETRO') IS NOT NULL
    DROP TABLE #tempUltiproExpensesRETRO
IF object_id('tempdb..#tempUltiproExpensesCPTO') IS NOT NULL
    DROP TABLE #tempUltiproExpensesCPTO 
 IF object_id('tempdb..#tempUltiproExpensesFINALLEAVE') IS NOT NULL
    DROP TABLE #tempUltiproExpensesFINALLEAVE   
IF object_id('tempdb..#tempUltiproExpensesRETRO') IS NOT NULL
    DROP TABLE #tempUltiproExpensesRETRO
IF object_id('tempdb..#tempUltiproExpensesRETRO_HOLIDAY') IS NOT NULL
    DROP TABLE #tempUltiproExpensesRETRO_HOLIDAY    
IF object_id('tempdb..#tempUltiproExpensesREFFERAL') IS NOT NULL
    DROP TABLE #tempUltiproExpensesREFFERAL
IF object_id('tempdb..#tempUltiproExpensesBONUS') IS NOT NULL
    DROP TABLE #tempUltiproExpensesBONUS   
IF object_id('tempdb..#tempUltiproExpensesANNIVERSITY') IS NOT NULL
    DROP TABLE #tempUltiproExpensesANNIVERSITY 
IF object_id('tempdb..#tempTotaLabor2') IS NOT NULL
    DROP TABLE #tempTotaLabor2 
    
IF object_id('tempdb..#tempMetrics010') IS NOT NULL
    DROP TABLE #tempMetrics010
IF object_id('tempdb..#tempMetrics020') IS NOT NULL
    DROP TABLE #tempMetrics020    
IF object_id('tempdb..#tempTotalDonorsByLocation') IS NOT NULL
    DROP TABLE #tempTotalDonorsByLocation
IF object_id('tempdb..#tempRevenueByLocation010') IS NOT NULL
    DROP TABLE #tempRevenueByLocation010 
IF object_id('tempdb..#tempbudget') IS NOT NULL
    DROP TABLE #tempbudget
IF object_id('tempdb..#tempLabor_KPIs_010') IS NOT NULL
    DROP TABLE #tempLabor_KPIs_010


SELECT	[SALSTERR] Terr_No, 
		[SLTERDSC] Territory, 
		rtrim([ACTNUMBR_2]) + '-' + rtrim([ACTDESCR]) Location_No_and_Description, 
		[ACTDESCR] Location, 
		[ACTNUMBR_2] Locn_No, 
		[ACTNUMBR_3] Territory_No, 
		[ACTIVE], 
		[INACTIVE]
INTO	#tempTerritoriesAndStores
FROM	[RM00303](NOLOCK) LEFT JOIN
		[GL00100](NOLOCK) ON [ACTNUMBR_3] = [SALSTERR]
WHERE	[ACTIVE] = 1 AND
		[ACTNUMBR_2] IN (@Locations)	
ORDER BY [SLTERDSC],[ACTDESCR]
--SELECT * FROM #tempTerritoriesAndStores
/*--------------------------------------------------[Labor Hours, Regular Hours and OverTime by Location]----------------------------------------------------*/
/*Create temp table to hold calculated overtime + regular work hours and strip Dept # to include training hours */
SELECT [EmployeeNumber]
      ,[Wage]
      ,CAST(([Hours]*[HourTypeMultiplier]) AS Decimal(18,2)) [Hours]--labor hours 
      , ([HourTypeMultiplier]) [OT]--multiplier 0, 1, 1.5 for overtime calculations 
      ,(case when [HourTypeMultiplier] != 1.5 and [HourTypeMultiplier] != 0 and TimeCode = 'WRK' then [Hours] else 0 end) Reg_Hours -- hours when equal to everything but 1.5 is regular hours 
      ,(case when [HourTypeMultiplier] = 1.5 then [Hours] else 0 end) Overtime--hours when equal 1.5 for overtime 
      ,(case when [TimeCode] = 'HOL' then [Hours] else 0 end) Holiday 
      ,(case when [TimeCode] = 'HWK' then [Hours] else 0 end) Holiday_Worked
      ,(case when [TimeCode] = 'ANNUAL' then [Hours] else 0 end) ANNUAL
      ,(case when [TimeCode] = '90DAY' then [Hours] else 0 end) NINTEY
      ,(case when [TimeCode] = 'BRV' then [Hours] else 0 end)BR_LEAVE
      ,(case when [TimeCode] = 'BWE' then [Hours] else 0 end) BAD_WEATHER
      ,(case when [TimeCode] = 'JURY' then [Hours] else 0 end) JURY
      --,(case when [TimeCode] = 'EARLY' then [Hours] else 0 end) EARLY_CLOCK
      --,(case when [TimeCode] = 'LATE' then [Hours] else 0 end) LATE_CLOCK
      ,(case when [TimeCode] = 'LEAVE' then [Hours] else 0 end) LEAVE
      ,(case when [TimeCode] = 'TRN' then [Hours] else 0 end) TRAINING
      ,(case when [TimeCode] = 'PTLEAVE' then [Hours] else 0 end) PARTTIME_LEAVE
      ,(case when [TimeCode] = 'ORIEN' then [Hours] else 0 end) ORIENTATION
      ,(case when [TimeCode] = 'NONWK' then [Hours] else 0 end) NON_WRK
      --,(case when [TimeCode] = 'LL' then [Hours] else 0 end) LATE  
      , Left([DepartmentNumber],4) [DepartmentNumber]
      ,[DepartmentName]
      ,[Date]
      ,[TimeCode]
      ,[Locn_No]
  INTO #payroll_import_calculate     
  FROM [Aztec_Payroll_Import], #tempTerritoriesAndStores 
  where (cast([Date] as datetime) between @BegDate and @EndDate)and Locn_No in (@Locations)
  order by Locn_No

SELECT	ap.[DepartmentNumber] Store
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then [Hours] else 0 end) [Labor_Hrs]      
		,Sum(case when cast([Date] as datetime)  between dateadd(YYYY,-1,@BegDate) and dateadd(YYYY,-1,@EndDate) then [Hours] else 0 end) [Labor_Hrs_LY]
		,Sum(case when cast([Date] as datetime)  between @BegDate and @EndDate then [Hours]*[Wage] else 0 end) [Labor_Exp]      
		,Sum(case when cast([Date] as datetime)  between dateadd(YYYY,-1,@BegDate) and dateadd(YYYY,-1,@EndDate) then [Hours]*[Wage] else 0 end) [Labor_Exp_LY]   
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then [Reg_Hours] else 0 end) [Reg_Hours]--sum regular hours in the date parameter
		,sum(case when cast([Date] as datetime) between @BegDate and @EndDate then [Overtime] else 0 end) [OverTime010]--sum overtime hours in the date parameter 
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then Holiday else 0 end) Holiday_Hours
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then NINTEY else 0 end) NINTEYS
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then ANNUAL else 0 end) ANNUALS
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then BR_LEAVE else 0 end) BR_LEAVES
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then Holiday_Worked else 0 end) Holiday_WorkedS
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then BAD_WEATHER else 0 end) BAD_WEATHERS
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then JURY else 0 end) JURYS
		--,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then EARLY_CLOCK else 0 end) EARLY_CLOCKS
		--,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then LATE_CLOCK else 0 end) LATE_CLOCKS
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then LEAVE else 0 end) LEAVES
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then TRAINING else 0 end) TRAININGS
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then PARTTIME_LEAVE else 0 end) PARTTIME_LEAVES
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then ORIENTATION else 0 end) ORIENTATIONS
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then NON_WRK else 0 end) NON_WRKS
		--,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then LATE else 0 end) LATES
		,(Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then TRAINING else 0 end) + Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then NINTEY else 0 end) + Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then ANNUAL else 0 end) + Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then ORIENTATION else 0 end)) [TRAINING_HOURS]--sum of all training hours
		,(Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then LEAVE else 0 end) + Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then PARTTIME_LEAVE else 0 end) + Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then BR_LEAVE else 0 end)) [LEAVE_HOURS]--sum of all leave hours 
INTO	#tempLaborHoursByLocation010      
FROM	#payroll_import_calculate ap JOIN
		#tempTerritoriesAndStores ts ON ts.Locn_No = ap.DepartmentNumber
WHERE	(cast([Date] as datetime) between dateadd(YYYY,-1,@BegDate) and @EndDate ) and  ap.[DepartmentNumber] in (@Locations) and ap.Locn_No = ts.Locn_No--compare location of 2 seperate temp tables to get hours by location 
GROUP BY [DepartmentNumber]
order by DepartmentNumber

INSERT INTO #tempLaborHoursByLocation010(Store, Labor_Hrs, Labor_Hrs_LY, Labor_Exp, Labor_Exp_LY, Reg_Hours,[OverTime010], Holiday_Hours,NINTEYS,ANNUALS,BR_LEAVES,Holiday_WorkedS, BAD_WEATHERS,JURYS,
/*EARLY_CLOCKS,LATE_CLOCKS,*/LEAVES,TRAININGS,PARTTIME_LEAVES,ORIENTATIONS,NON_WRKS/*LATES*/)
SELECT	Locn_No, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0
FROM	#tempTerritoriesAndStores
WHERE	Locn_No NOT IN (SELECT Store FROM #tempLaborHoursByLocation010)

SELECT GL1.ACTINDX as ACTINDX1,
	ACTNUMBR_1 As Account_Number,
	ACTNUMBR_2 as Locn_No,
	ACTNUMBR_3,
	DSCRIPTN,
	TRXDATE,
	DEBITAMT 
  into #tempGSG
  FROM [GWC].[dbo].[GL20000] GL1 left join [GWC].[dbo].[GL00100] GL2 on gl1.ACTINDX = gl2.ACTINDX
  where ACTNUMBR_1 = '607' and TRXDATE between @BegDate and @EndDate and ACTNUMBR_2 in (@Locations)
  order by ACTNUMBR_2, TRXDATE
/*  
select ACTINDX1,
	   Account_Number,
	   Locn_No,
	   TRXDATE,
	   DEBITAMT 
into #tempGSG1	  
from #tempGSG
order by 
--select * from #tempGSG1
*/
Select ACTINDX1,
		Account_Number,
		Locn_No,
		sum(DEBITAMT) as finalAmount
into #tempGSG2
from #tempGSG
group by ACTINDX1, Account_Number,Locn_No
--select * from  #tempGSG2

Select  Locn_No
		,[Company Code]
      ,[ACTNUMBR_2]
      ,[Store Location]
      ,[ACTNUMBR_1]
      ,[Org Level 3]
      ,[Employee Name]
      ,[Employee Number]
      ,[Earnings Code]
      ,[Hours]
      ,[Amount]
      ,[Pay Date]
      ,[Period Start Date]
      ,[Period End Date]
Into #tempCPTO
FROM [GWC].[dbo].[Payroll_Expenses] PE left join #tempTerritoriesAndStores ts on pe.ACTNUMBR_2 = ts.Locn_No
where [Period End Date] between @BegDate and @EndDate and ACTNUMBR_2 in (@Locations)
--Select * from #tempCPTO

Select 
		case when [Earnings Code] = 'CPTO'  then sum([Amount]) else 0 end [CPTOLeave]
		,case when [Earnings Code] = 'RETRO' and [ACTNUMBR_1] between '701' and '706' then sum([Amount]) else 0 end [RETRO]
		,case when [Earnings Code] = 'REHOL' and [ACTNUMBR_1] between '701' and '707' then sum([Amount]) else 0 end [RETRO_HOLIDAY]
		,case when [Earnings Code] = 'REFFE' and [ACTNUMBR_1] between '701' and '708' then sum([Amount]) else 0 end [REFFERAL_BONUS]
		,case when [Earnings Code] = 'BON' and [ACTNUMBR_1] between '701' and '709' then sum([Amount]) else 0 end [BONUS]
		,case when [Earnings Code] = 'ANNIV' and [ACTNUMBR_1] between '701' and '710' then sum([Amount]) else 0 end [ANNIVERSARY]
Into #tempUltiproExpenses
FROM #tempCPTO
group by [Earnings Code], [ACTNUMBR_1]
--Select * from #tempUltiproExpenses

Select Locn_No ,case when sum([Amount]) IS null then 0 else sum([Amount]) end  [RETRO]
Into #tempUltiproExpensesRETRO
FROM #tempCPTO 
WHERE [ACTNUMBR_1] between '701' and '706' AND [Earnings Code] = 'RETRO'
group by Locn_No
--Select * from #tempUltiproExpensesRETRO

Select Locn_No ,case when sum([Amount]) IS null then 0 else sum([Amount]) end [RETRO_HOLIDAY]
Into #tempUltiproExpensesRETRO_HOLIDAY
FROM #tempCPTO 
WHERE [ACTNUMBR_1] between '701' and '707' AND [Earnings Code] = 'REHOL'
group by Locn_No
--Select * from #tempUltiproExpensesRETRO_HOLIDAY

Select Locn_No ,case when sum([Amount]) IS null then 0 else sum([Amount]) end  [REFFERAL]
Into #tempUltiproExpensesREFFERAL
FROM #tempCPTO 
WHERE [ACTNUMBR_1] between '701' and '708' AND [Earnings Code] = 'REFFE'
group by Locn_No
--Select * from #tempUltiproExpensesREFFERAL

Select Locn_No ,case when sum([Amount]) IS null then 0 else sum([Amount]) end  [BONUS]
Into #tempUltiproExpensesBONUS
FROM #tempCPTO
WHERE [ACTNUMBR_1] between '701' and '709' AND [Earnings Code] = 'BON'
group by Locn_No
--Select * from #tempUltiproExpensesBONUS

Select Locn_No ,case when sum([Amount]) IS null then 0 else sum([Amount]) end  [ANNIV]
Into #tempUltiproExpensesANNIVERSITY
FROM #tempCPTO 
WHERE [ACTNUMBR_1] between '701' and '710' AND [Earnings Code] = 'ANNIV'
group by Locn_No
Select Locn_No ,case when sum([Amount]) IS null then 0 else sum([Amount]) end  [CPTO]
Into #tempUltiproExpensesCPTO
FROM #tempCPTO 
WHERE [Earnings Code] = 'CPTO'
group by Locn_No
Select Locn_No ,case when sum([Amount]) IS null then 0 else sum([Amount]) end  [FINALLEAVE]
Into #tempUltiproExpensesFINALLEAVE
FROM #tempCPTO 
WHERE [Earnings Code] = 'FLEA'
group by Locn_No
Select Locn_No ,case when sum([Amount]) IS null then 0 else sum([Amount]) end  [GRANDFATHER]
Into #tempUltiproExpensesGRANDFATHER
FROM #tempCPTO 
WHERE [Earnings Code] = 'HOLG'
group by Locn_No
--Select * from #tempUltiproExpensesFINALLEAVE
SELECT	Store,
		Location_No_and_Description,
		[Labor_Hrs],[Labor_Hrs_LY],
		Labor_Exp,
		Labor_Exp_LY,
		finalAmount,
		case when finalAmount is null then labor_exp else (Labor_Exp + finalAmount) end runningTotal
INTO	#tempTotalLabor          
FROM	#tempTerritoriesAndStores tg left join #tempLaborHoursByLocation010 tl on tg.Locn_No = tl.Store  left join #tempGSG2 ts on tl.Store = ts.Locn_No
INSERT INTO #tempTotalLabor(Store, Location_No_and_Description, Labor_Hrs, Labor_Hrs_LY, Labor_Exp, Labor_Exp_LY, finalAmount, runningTotal)
SELECT	Locn_No, Location_No_and_Description, 0, 0, 0, 0, 0, 0
FROM	#tempTerritoriesAndStores
WHERE	Locn_No NOT IN (SELECT LEFT(Location_No_and_Description, 4) FROM #tempTotalLabor   WHERE Location_No_and_Description IS NOT NULL)
--select * from #tempTotalLabor

SELECT	tl.Store,
		Location_No_and_Description, 
		case when RETRO is null then 0 else RETRO end [RETRO],
		case when RETRO_HOLIDAY Is null then 0 else RETRO_HOLIDAY end [RETRO_HOLIDAY],
		CASE when REFFERAL Is null then 0 else REFFERAL end [REFFERAL],
		CASE WHEN BONUS IS NULL THEN 0 ELSE BONUS END [BONUS],
		CASE WHEN ANNIV IS NULL THEN 0 ELSE ANNIV END [ANNIV],
		CASE WHEN CPTO IS NULL THEN 0 ELSE CPTO END [CPTO],
		CASE WHEN FINALLEAVE IS NULL THEN 0 ELSE FINALLEAVE END [FINALLEAVE],
		CASE WHEN GRANDFATHER IS NULL THEN 0 ELSE GRANDFATHER END [GRANDFATHER],
		(runningTotal + CASE WHEN ANNIV IS NULL THEN 0 ELSE ANNIV END  + CASE WHEN BONUS IS NULL THEN 0 ELSE BONUS END + CASE when REFFERAL Is null then 0 else REFFERAL end + case when RETRO_HOLIDAY Is null then 0 else RETRO_HOLIDAY end +  case when RETRO is null then 0 else RETRO end + CASE WHEN CPTO IS NULL THEN 0 ELSE CPTO END + CASE WHEN FINALLEAVE IS NULL THEN 0 ELSE FINALLEAVE END + CASE WHEN GRANDFATHER IS NULL THEN 0 ELSE GRANDFATHER END ) [ULTIPRO_TOTAL] 
INTO	#tempTotaLabor2     
FROM	#tempTotalLabor tl left join #tempUltiproExpensesBONUS tb on tl.store = tb.Locn_No left join #tempUltiproExpensesREFFERAL tr on tl.Store = tr.Locn_No left join #tempUltiproExpensesRETRO uer on tl.Store = uer.Locn_No left join #tempUltiproExpensesRETRO_HOLIDAY UERH on tl.Store = UERH.Locn_No left join #tempUltiproExpensesANNIVERSITY an on  tl.Store = an.Locn_No left join #tempLaborHoursByLocation010 th on tl.store = th.Store left join #tempUltiproExpensesCPTO cp on tl.Store = cp.Locn_No left join #tempUltiproExpensesFINALLEAVE fl on fl.Locn_No = tl.Store LEFT JOIN #tempUltiproExpensesGRANDFATHER TG ON TG.Locn_No = TL.Store
INSERT INTO #tempTotaLabor2 (tl.Store, Location_No_and_Description, RETRO, RETRO_HOLIDAY, REFFERAL, BONUS,ANNIV,CPTO, ULTIPRO_TOTAL, FINALLEAVE)
SELECT	Locn_No, Location_No_and_Description, 0, 0, 0, 0,0,0,0, 0
FROM	#tempTerritoriesAndStores
WHERE	Locn_No NOT IN (SELECT LEFT(Location_No_and_Description, 4) FROM #tempTotalLabor   WHERE Location_No_and_Description IS NOT NULL)
--select * from #tempTotaLabor2

SELECT	Location_No_and_Description
		,'Regular Hours' [Description]
		,Reg_Hours [Amount]--hours amount per location 
		,01 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION 
SELECT	Location_No_and_Description
		,'OverTime Hours' [Description]
		,OverTime010 [Amount]--hours amount per location 
		,02 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION
SELECT	Location_No_and_Description
		,'Training Hours' [Description]
		,TRAININGS [Amount]--hours amount per location 
		,03 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION 
SELECT	Location_No_and_Description
		,'90 Day Hours' [Description]
		,NINTEYS [Amount]--hours amount per location 
		,04 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION
SELECT	Location_No_and_Description
		,'Annual Hours' [Description]
		,ANNUALS [Amount]--hours amount per location 
		,05 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION 
SELECT	Location_No_and_Description
		,'Orientation Hours' [Description]
		,ORIENTATIONS [Amount]--hours amount per location 
		,06 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION 
SELECT	Location_No_and_Description
		,'Leave Hours' [Description]
		,LEAVES [Amount]--hours amount per location 
		,07 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION
SELECT	Location_No_and_Description
		,'Part-Time Leave Hours' [Description]
		,PARTTIME_LEAVES [Amount]--hours amount per location 
		,08 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION
SELECT	Location_No_and_Description
		,'Bereavement leave Hours' [Description]
		,BR_LEAVES [Amount]--hours amount per location 
		,09 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION
SELECT	Location_No_and_Description
		,'Holiday Hours' [Description]
		,Holiday_Hours [Amount]--hours amount per location 
		,10 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION 
SELECT	Location_No_and_Description
		,'Holiday Worked Paid Hours' [Description]
		,Holiday_WorkedS [Amount]--hours amount per location 
		,11 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION 
SELECT	Location_No_and_Description
		,'Bad Weather Day Hours' [Description]
		,BAD_WEATHERS [Amount]--hours amount per location 
		,12 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION
SELECT	Location_No_and_Description
		,'Jury Duty Hours' [Description]
		,JURYS [Amount]--hours amount per location 
		,13 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION 
/*
SELECT	Location_No_and_Description
		,'Early Clock In Hours' [Description]
		,EARLY_CLOCKS [Amount]--hours amount per location 
		,14 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION
SELECT	Location_No_and_Description
		,'Late Clock Out Hours' [Description]
		,LATE_CLOCKS [Amount]--hours amount per location 
		,15 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION 
*/
SELECT	Location_No_and_Description
		,'Non-Work Paid Time Hours' [Description]
		,NON_WRKS [Amount]--hours amount per location 
		,16 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION
/*
SELECT	Location_No_and_Description
		,'Late Hours' [Description]
		,LATES [Amount]--hours amount per location 
		,17 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION 
*/
SELECT	Location_No_and_Description
		,'Total Labor Hours' [Description]
		,Labor_Hrs [Amount]--hours amount per location 
		,18 [Seq]
		,'Decimal' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION
SELECT	Location_No_and_Description
		,'Sub-Total Timesheet Expense ' [Description]
		,Labor_Exp [Amount]--hours amount per location 
		,19 [Seq]
		,'Money' [Value_Format]
FROM	#tempLaborHoursByLocation010 tl  left join #tempTerritoriesAndStores ts on tl.Store = ts.Locn_No
UNION
SELECT	Location_No_and_Description
		,'Contract Labor Expense' [Description]
		,finalAmount [Amount]--hours amount per location 
		,20 [Seq]
		,'Money' [Value_Format]
FROM	#tempTotalLabor 
UNION
SELECT	Location_No_and_Description
		,'CPTO Expense' [Description]
		,CPTO [Amount]--hours amount per location 
		,21 [Seq]
		,'Money' [Value_Format]
FROM	#tempTotaLabor2   
UNION
SELECT	Location_No_and_Description
		,'Retro Labor Expense' [Description]
		,case when Retro IS null then 0 else RETRO end [Amount]--hours amount per location 
		,22 [Seq]
		,'Money' [Value_Format]
FROM	#tempTotaLabor2   
UNION
SELECT	Location_No_and_Description
		,'Retro Holiday Expense' [Description]
		,case when RETRO_HOLIDAY IS null then 0 else RETRO_HOLIDAY end[Amount]--hours amount per location 
		,23 [Seq]
		,'Money' [Value_Format]
FROM	#tempTotaLabor2   
UNION
SELECT	Location_No_and_Description
		,'Referral Expense' [Description]
		,case when REFFERAL IS null then 0 else REFFERAL end [Amount]--hours amount per location 
		,24 [Seq]
		,'Money' [Value_Format]
FROM	#tempTotaLabor2    
UNION
SELECT	Location_No_and_Description
		,'Bonus Expense' [Description]
		,case when BONUS IS null then 0 else BONUS end[Amount]--hours amount per location 
		,25 [Seq]
		,'Money' [Value_Format]
FROM	#tempTotaLabor2   
UNION
SELECT	Location_No_and_Description
		,'Anniversary Expense' [Description]
		,case when ANNIV IS null then 0 else ANNIV end [Amount]--hours amount per location 
		,26 [Seq]
		,'Money' [Value_Format]
FROM	#tempTotaLabor2   
UNION
SELECT	Location_No_and_Description
		,'Final Leave Expense' [Description]
		,case when FINALLEAVE IS null then 0 else FINALLEAVE end [Amount]--hours amount per location 
		,27 [Seq]
		,'Money' [Value_Format]
FROM	#tempTotaLabor2   
UNION
SELECT	Location_No_and_Description
		,'Grandfathered Holiday' [Description]
		,case when GRANDFATHER IS null then 0 else GRANDFATHER end [Amount]--hours amount per location 
		,28 [Seq]
		,'Money' [Value_Format]
FROM	#tempTotaLabor2   
UNION
SELECT	Location_No_and_Description
		,'Total Labor Expense' [Description]
		, ULTIPRO_TOTAL [Amount]--hours amount per location 
		,29 [Seq]
		,'Money' [Value_Format]
FROM	#tempTotaLabor2 

order by Seq