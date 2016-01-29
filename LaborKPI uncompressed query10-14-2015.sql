--/*
--Test Parameters:
declare @BegDate datetime
declare @EndDate datetime
declare @Locations varchar(max)
declare @BudgetID varchar(max)

set @BegDate = '10/14/15'
set @EndDate = '10/14/15'
set @BudgetID = 'BUDGET 2015'
set @Locations = '1212'

--*/
IF object_id('tempdb..#tempTotalDonorsByLocation') IS NOT NULL DROP TABLE #tempTotalDonorsByLocation 
IF object_id('tempdb..#tempLaborHoursByLocation010') IS NOT NULL DROP TABLE #tempLaborHoursByLocation010 
IF object_id('tempdb..#tempTerritoriesAndStores') IS NOT NULL DROP TABLE #tempTerritoriesAndStores 
IF object_id('tempdb..#tempMetrics010') IS NOT NULL DROP TABLE #tempMetrics010 
IF object_id('tempdb..#tempMetrics020') IS NOT NULL DROP TABLE #tempMetrics020 
IF object_id('tempdb..#tempRevenueByLocation010') IS NOT NULL DROP TABLE #tempRevenueByLocation010 
IF object_id('tempdb..#tempHeaderAccounts010') IS NOT NULL DROP TABLE #tempHeaderAccounts010
IF object_id('tempdb..#tempLabor_KPIs_010') IS NOT NULL DROP TABLE #tempLabor_KPIs_010
/*----------------------------------------------[Get Payroll Metrics]----------------------------------------*/
SELECT	glam.[ACTINDX]
		,[GL_Acct]
		,[Locn]
		,[AC]
		,[Account_Description]
		,[ACTALIAS]
		,Max(gltv.[DEX_ROW_ID]) [DEX_ROW_ID]
INTO	#tempMetrics010
FROM	dbo.GL00100 glam left join
		[aztec_GL_Transactions010] gltv ON gltv.[ACTINDX] =  glam.[ACTINDX]
where	--[GL_Acct] like '9__-0000-00'
		ACTNUMBR_1 like '9__'
		AND ACTNUMBR_1 in ('901','912','913','986','987','988')
		AND ACTNUMBR_2 like '0000'
		AND ACTNUMBR_3 like '00'
		AND ACCTTYPE = 2
group BY glam.[ACTINDX],[GL_Acct],[Locn],[AC],[Account_Description],[ACTALIAS]
--SELECT * FROM #tempMetrics010
----------------Use temp table #tempMetrics010 data to pull latest Unit Account Transaction from GL Trans View----------------------
SELECT	gltv.[ACTINDX] Metric_ACTINDX
		,gltv.[GL_Acct]Metric_GL_Acct
		,gltv.[Locn] Metric_Locn
		,gltv.[AC] Metric_AC
		,gltv.[Account_Description] Metric_Account_Description
		,gltv.[TRXDATE] Metric_Date
		,rtrim(gltv.[Account_Description])+' ('+cast(cast(gltv.[Amount] as numeric (8,2))as varchar)+')' Metric
		,gltv.[Amount] Metric_Value
		,[ACTALIAS]
INTO	#tempMetrics020
FROM	#tempMetrics010 lm left join
		[aztec_GL_Transactions010] gltv on lm.[DEX_ROW_ID] = gltv.[DEX_ROW_ID] and lm.[ACTINDX] = gltv.[ACTINDX]
--Select * from #tempMetrics020
/*-----------------[Get Header GL Account Descriptions (excludes location description)]-------------*/ 
SELECT	[ACTNUMBR_1] Header_AC_No, 
		[ACTDESCR] Header_Acct_Descr,
		[ACTIVE]
INTO	#tempHeaderAccounts010
FROM	[GL00100](NOLOCK)
WHERE	[PSTNGTYP] = 1 AND 
		[ACTNUMBR_1] between '400' and '599' and [ACTNUMBR_1] != '509' AND  
		[ACTNUMBR_2] LIKE '0000'
/*--------------------------------[Get Territory and Location List]------------------------------------*/
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
      ,(case when [TimeCode] = 'EARLY' then [Hours] else 0 end) EARLY_CLOCK
      ,(case when [TimeCode] = 'LATE' then [Hours] else 0 end) LATE_CLOCK
      ,(case when [TimeCode] = 'LEAVE' then [Hours] else 0 end) LEAVE
      ,(case when [TimeCode] = 'TRN' then [Hours] else 0 end) TRAINING
      ,(case when [TimeCode] = 'PTLEAVE' then [Hours] else 0 end) PARTTIME_LEAVE
      ,(case when [TimeCode] = 'ORIEN' then [Hours] else 0 end) ORIENTATION
      ,(case when [TimeCode] = 'NONWK' then [Hours] else 0 end) NON_WRK
      ,(case when [TimeCode] = 'LL' then [Hours] else 0 end) LATE  
      , Left([DepartmentNumber],4) [DepartmentNumber]
      ,[DepartmentName]
      ,[Date]
      ,[TimeCode]
      ,[Locn_No]
  INTO #payroll_import_calculate     
  FROM [Aztec_Payroll_Import], #tempTerritoriesAndStores 
  where (cast([Date] as datetime) between @BegDate and @EndDate)
--select * from #payroll_import_calculate
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
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then EARLY_CLOCK else 0 end) EARLY_CLOCKS
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then LATE_CLOCK else 0 end) LATE_CLOCKS
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then LEAVE else 0 end) LEAVES
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then TRAINING else 0 end) TRAININGS
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then PARTTIME_LEAVE else 0 end) PARTTIME_LEAVES
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then ORIENTATION else 0 end) ORIENTATIONS
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then NON_WRK else 0 end) NON_WRKS
		,Sum(case when cast([Date] as datetime) between @BegDate and @EndDate then LATE else 0 end) LATES
INTO	#tempLaborHoursByLocation010      
FROM	#payroll_import_calculate ap JOIN
		#tempTerritoriesAndStores ts ON ts.Locn_No = ap.DepartmentNumber
WHERE	(cast([Date] as datetime) between dateadd(YYYY,-1,@BegDate) and @EndDate ) and ap.Locn_No = ts.Locn_No--compare location of 2 seperate temp tables to get hours by location 
GROUP BY [DepartmentNumber]
INSERT INTO #tempLaborHoursByLocation010(Store, Labor_Hrs, Labor_Hrs_LY, Labor_Exp, Labor_Exp_LY, Reg_Hours,[OverTime010], Holiday_Hours,NINTEYS,ANNUALS,BR_LEAVES,Holiday_WorkedS, BAD_WEATHERS,JURYS,
EARLY_CLOCKS,LATE_CLOCKS,LEAVES,TRAININGS,PARTTIME_LEAVES,ORIENTATIONS,NON_WRKS,LATES)
SELECT	Locn_No, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
FROM	#tempTerritoriesAndStores
WHERE	Locn_No NOT IN (SELECT Store FROM #tempLaborHoursByLocation010)
--Select * from #tempLaborHoursByLocation010 
/*--------------------------------------[Donor Unit Account Totals by Store for specified date range]---------------------------------------------*/
SELECT	[Terr_No], 
		[Territory], 
		[Location_No_and_Description], 
		[Locn]/*,[Account_Description]*/ , 
		sum([Amount]) Total_Donors
INTO	#tempTotalDonorsByLocation
FROM	[aztec_GL_Transactions010](NOLOCK) glt JOIN
		#tempTerritoriesAndStores tas ON tas.Locn_No = glt.Locn
WHERE	[TRXDATE] BETWEEN @BegDate AND @EndDate AND 
		[Account_Type] LIKE '2%' 
		AND AC IN (907, 909, 920)
GROUP BY [Terr_No], [Territory], [Location_No_and_Description], [Locn]/*,[Account_Description]*/ 
ORDER BY Locn  
INSERT INTO #tempTotalDonorsByLocation(Terr_No, Territory, Location_No_and_Description, Locn, Total_Donors)
SELECT	Terr_No, Territory, Location_No_and_Description, Locn_No, 0
FROM #tempTerritoriesAndStores
WHERE Locn_No NOT IN (SELECT Locn FROM #tempTotalDonorsByLocation)
--Test query: select '#tempTotalDonorsByLocation' tbl,* from #tempTotalDonorsByLocation
/*----------------------------------[Textile Unit Account Totals by Store for specified date range]----------------------------------*/
SELECT	[Terr_No], 
		[Territory], 
		[Location_No_and_Description], 
		[Locn]/*,[Account_Description]*/ , 
		sum([Amount]) Total_Textile--used to get the total textile hung per store 
INTO	#tempTotalTextileByLocation
FROM	[aztec_GL_Transactions010](NOLOCK) glt JOIN
		#tempTerritoriesAndStores tas ON tas.Locn_No = glt.Locn
WHERE	[TRXDATE] BETWEEN @BegDate AND @EndDate AND 
		[Account_Type] LIKE '2%' 
		AND AC IN ('912')--access textile hung 
GROUP BY [Terr_No], [Territory], [Location_No_and_Description], [Locn]/*,[Account_Description]*/ 
ORDER BY Locn  
INSERT INTO #tempTotalTextileByLocation(Terr_No, Territory, Location_No_and_Description, Locn, Total_Textile)
SELECT	Terr_No, Territory, Location_No_and_Description, Locn_No, 0
FROM #tempTerritoriesAndStores
WHERE Locn_No NOT IN (SELECT Locn FROM #tempTotalTextileByLocation)--add in un used locations 
--select '#tempTotalTextileByLocation' tbl,* from #tempTotalDonorsByLocation
/*---------------------------------------------------------[Revenue by Location, by Acct]---------------------------------------------*/ 
SELECT	[Locn] Rev_Locn, 
		--- sum([Amount]) Rev_Amount,
		-Sum(case when [TRXDATE] between @BegDate and @EndDate then [Amount] else 0 end) [Sales],
		-Sum(case when [TRXDATE] between dateadd(YYYY,-1,@BegDate) and dateadd(YYYY,-1,@EndDate) then [Amount] else 0 end) [Sales_LY]
INTO	#tempRevenueByLocation010
FROM	#tempHeaderAccounts010 ha /*Header Accounts*/ LEFT JOIN
		[aztec_GL_Transactions010] gltv(NOLOCK) ON ha.[Header_AC_No] = gltv.[AC] JOIN
		#tempTerritoriesAndStores ts ON ts.Locn_No = gltv.Locn
WHERE	[AC] BETWEEN '400' AND '599' AND [AC] != '509'
		AND ([TRXDATE] BETWEEN  dateadd(YYYY,-1,@BegDate) AND @EndDate) 
GROUP BY [Locn]
ORDER BY [Locn]
--select * from #tempRevenueByLocation010
SELECT  [BUDGETID]
      ,[ACTNUMBR_1]
      ,[ACTNUMBR_2]
      ,[PERIODDT]
      ,[PERIODID] 
	   ,([BUDGETAMT]) Budgets
      ,[YEAR1]
      ,Locn_No
  into #tempbudget 
  FROM [GL00201],#tempTerritoriesAndStores
  where  Locn_No = [ACTNUMBR_2] and ACTNUMBR_2  in (@Locations)
  and [PERIODID] between month (@BegDate) and month(@EndDate)
  and ACTNUMBR_1 = '701'  and BUDGETID = 'BUDGET 2015'
  order by BUDGETID, ACTNUMBR_2
--select * from #tempbudget
SELECT distinct * 
into #tempbudget01    
FROM  STORE_RPT_BudgetPct 
where DATE between (DATEADD(dd,-(DAY(@BegDate)-1),@BegDate)) and (DATEADD(dd,-(DAY(DATEADD(mm,1,@EndDate))),DATEADD(mm,1,@EndDate)))
order by DATE 
--selects the budget amounts, budgetid, locnNo, and the AC to be used to calculate the buget---
SELECT distinct 
		ACTNUMBR_2 LocnNo,
		ACTNUMBR_1,
		gl.PERIODID,
		(-budgetamt) BudgetAMT,
		rtrim(gl.[BUDGETID]) BudgetID
into #tempbudget02 
FROM GL00201 gl , #tempbudget01
WHERE ([ACTNUMBR_1] between '400' and '599' and [ACTNUMBR_1] != '509')
  and gl.[BUDGETID] = (@BudgetID)
  and [ACTNUMBR_2] in (@Locations)
  and gl.[PERIODID] between month (@BegDate) and month(@EndDate)
GROUP BY ACTNUMBR_2,ACTNUMBR_1,gl.PERIODID,BUDGETAMT,gl.BUDGETID
--budget query--
--sums all of the budgets together per store. 
select distinct 
				LocnNo,PERIODID,sum(BudgetAMT) BudgetAmount,BUDGETID
into #tempbudget03
from  #tempbudget02 
where [PERIODID] between month (@BegDate) and month(@EndDate)
group by LocnNo,PERIODID,BUDGETID
--table to multiply the budgetamount*dailybudpct for the daily amount totals--
select 
	LocnNo,tb03.PERIODID,DATE,MONTH,DailyBudPct,BudgetAmount,--the total budget amount for the month to be multiplied by the pct
	BudgetAmount *DailyBudPct [BudgetAmount1],--multiply the amount*pct for the time frame inside of the month 
	tb03.BUDGETID--budgetid from the gl00201 table
into #tempbudget04
from #tempbudget03 tb03,  #tempbudget01 
where MONTH(date) = tb03.PERIODID--distinguish when the months change to change the multiplier--
--sum all the daily amount totalsby month together for a monthly total budget--
select 
	LocnNo,PERIODID,SUM( BudgetAmount1) totalBudget,BUDGETID
into #tempbudget05
from #tempbudget04
where [PERIODID] between month (@BegDate) and month(@EndDate)--seperates by the month
group by  LocnNo,PERIODID,BUDGETID
select LocnNo,
	sum(totalBudget) finalBudget,--sum the total if the amount spans across several months. 
	tb05.BUDGETID
into #tempBudget06
from #tempbudget05 tb05

group by  LocnNo,tb05.BudgetID
--select * from #tempBudget06
/*--------------------------------------------[Labor Query: Labor Hrs/Donor and Sales/Labor Hr and Textile/donor and textile/labor hr]--------------------------*/
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

SELECT	Store,DL.Location_No_and_Description,[Labor_Hrs],[Labor_Hrs_LY] ,[Sales],[Sales_LY],Labor_Exp ,Labor_Exp_LY,Total_Donors,Budgets,Reg_Hours,[OverTime010],Holiday_Hours,Holiday_WorkedS,NINTEYS,NON_WRKS,ANNUALS,LATES,BR_LEAVES,BAD_WEATHERS,JURYS,EARLY_CLOCKS,LATE_CLOCKS,LEAVES,TRAININGS,ORIENTATIONS,PARTTIME_LEAVES,finalBudget,finalAmount
		,(TRAININGS + NINTEYS + ANNUALS + ORIENTATIONS) [TRAINING_HOURS]--sum of all training hours
		,(LEAVES + PARTTIME_LEAVES + BR_LEAVES) [LEAVE_HOURS]--sum of all leave hours 
		,CASE WHEN [Total_Donors] IS NOT NULL AND [Total_Donors] <>0 THEN [Labor_Hrs]/[Total_Donors] ELSE 0 END [Labor_Hrs_Per_Donor]
		,CASE WHEN  [Labor_Hrs]  IS NOT NULL AND [Labor_Hrs]<> 0 THEN [Sales]/[Labor_Hrs] ELSE 0 END [Sales_Per_Labor_Hr]
		,case when [Labor_Hrs_LY] IS NOT NULL AND [Labor_Hrs_LY]<> 0then [Sales_LY]/[Labor_Hrs_LY] else 0 end [Sales_Per_Labor_Hr_LY] 
		,case when (case when [Labor_Hrs_LY] IS NOT NULL AND [Labor_Hrs_LY]<>0 then [Sales_LY]/[Labor_Hrs_LY] else 0 end)<>0 then case when [Labor_Hrs] <>0 then [Sales]/[Labor_Hrs] else 0 end/case when [Labor_Hrs_LY]<>0 then [Sales_LY]/[Labor_Hrs_LY] else 0 end else 0 end [Pct_Chg_Sls_Per_Lbr_Hr_LY]
		,case when [Sales]<>0 and [Sales] is not null then Labor_Exp/[Sales]   else 0 end [PR_Exp_As_Pct_Sales] 
		,case when labor_exp <> 0 and labor_exp is not null then (([Budgets]-Labor_Exp)) else 0 end [Payroll_Var] 
		,case when [Sales_LY]<>0  and [Sales_LY] is not null then Labor_Exp_LY/[Sales_LY]  else 0 end [PR_Exp_As_Pct_Sales_LY]
		,CASE WHEN [Total_Donors] IS NOT NULL AND [Total_Donors] <>0 THEN ([Total_Textile]/[Total_Donors]) ELSE 0 END [TextileCalc] --calculation for textile hung per donor    
		,CASE WHEN  [Labor_Hrs]  IS NOT NULL AND [Labor_Hrs]<> 0 THEN ([Total_Textile]/[Labor_Hrs]) ELSE 0 END [TextileHungPerLaborCalc] --calculation for textile hung per labor hour
		,CASE WHEN  finalBudget IS NOT NULL AND finalBudget <> 0 THEN (Budgets/finalBudget) ELSE 0 END [LaborBudgetPercent]
		,((CASE WHEN  finalBudget  IS NOT NULL AND finalBudget <> 0 THEN (Budgets/finalBudget) ELSE 0 END ) * Sales ) [LaborBudgetCalc]
		,Labor_Exp - ((CASE WHEN  finalBudget  IS NOT NULL AND finalBudget <> 0 THEN (Budgets/finalBudget) ELSE 0 END ) * Sales ) [LaborBudgetVarianceCalc]
		,case when finalAmount is null then labor_exp else (Labor_Exp + finalAmount) end runningTotal
INTO	#tempLabor_KPIs_010           
FROM	#tempbudget tb,#tempTotalTextileByLocation tk,#tempTotalDonorsByLocation DL  -- Donors by Loc
		left join #tempLaborHoursByLocation010   LH ON LH.Store = DL.Locn  -- Labor Hrs
		left join #tempRevenueByLocation010 ON Store = Rev_Locn left join #tempBudget06 tb6 on tb6.LocnNo = Rev_Locn left join #tempGSG2 tg on tg.Locn_No = tb6.LocnNo
where dl.[Locn] = tk.[Locn]  and tb.Locn_No = dl.Locn --****compare location to make sure numbers change as store location change for both sales transactions and total textile 
INSERT INTO #tempLabor_KPIs_010(Store, Location_No_and_Description, Labor_Hrs, Labor_Hrs_Per_Donor, Sales_Per_Labor_Hr, Labor_Hrs_LY, 
							Sales_Per_Labor_Hr_LY,[Pct_Chg_Sls_Per_Lbr_Hr_LY], Sales, Sales_LY, Labor_Exp, Labor_Exp_LY, Total_Donors,
							PR_Exp_As_Pct_Sales, Payroll_Var, PR_Exp_As_Pct_Sales_LY,[TextileCalc],[TextileHungPerLaborCalc], Reg_Hours, [OverTime010],Holiday_Hours,Holiday_WorkedS,
							NINTEYS,NON_WRKS,ANNUALS,LATES,BR_LEAVES,BAD_WEATHERS,JURYS,EARLY_CLOCKS,LATE_CLOCKS,LEAVES,TRAININGS,ORIENTATIONS,PARTTIME_LEAVES,TRAINING_HOURS,LEAVE_HOURS,Budgets,LaborBudgetPercent,LaborBudgetCalc,LaborBudgetVarianceCalc,finalBudget,finalAmount,runningTotal)
SELECT	Locn_No, Location_No_and_Description, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
FROM	#tempTerritoriesAndStores
WHERE	Locn_No NOT IN (SELECT LEFT(Location_No_and_Description, 4) FROM #tempLabor_KPIs_010 WHERE Location_No_and_Description IS NOT NULL)
--Select * from #tempLabor_KPIs_010 ORDER BY Store



/*---------------------------------------------------------------[Labor_Hrs_Per_Donor]-----------------------------------------------------*/
 /*select statement for the final temp table to display after the unions*/  
SELECT	Location_No_and_Description
		,'Regular Hours' [Description]
		,0 [Metric_Value]
		,Reg_Hours [Amt]--hours amount per location 
		,1 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'
UNION 
SELECT	Location_No_and_Description
		,'OverTime Hours' [Description]
		,0 [Metric_Value]
		,[OverTime010] [Amt]--overtime per location
		,2 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'
UNION 
SELECT	Location_No_and_Description
		,'Training Hours' [Description]
		,0 [Metric_Value]
		,TRAININGs [Amt]--hours amount per location 
		,3 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'
UNION 
SELECT	Location_No_and_Description
		,'90 Day Hours' [Description]
		,0 [Metric_Value]
		,NINTEYS [Amt]--hours amount per location 
		,4 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'
UNION 
SELECT	Location_No_and_Description
		,'Annual Hours' [Description]
		,0 [Metric_Value]
		,ANNUALS [Amt]--hours amount per location 
		,5 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'
UNION 
SELECT	Location_No_and_Description
		,'Orientation Hours' [Description]
		,0 [Metric_Value]
		,ORIENTATIONS [Amt]--hours amount per location 
		,6 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'
UNION 
SELECT	Location_No_and_Description
		,'Leave Hours' [Description]
		,0 [Metric_Value]
		,leaves [Amt]--hours amount per location 
		,7 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'
UNION 
SELECT	Location_No_and_Description
		,'Part-Time Leave Hours' [Description]
		,0 [Metric_Value]
		,PARTTIME_LEAVES [Amt]--hours amount per location 
		,8 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'
UNION 
SELECT	Location_No_and_Description
		,'Breavement Leave Hours' [Description]
		,0 [Metric_Value]
		,BR_LEAVES [Amt]--hours amount per location 
		,9 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'
UNION 
SELECT	Location_No_and_Description
		,'Holiday Hours' [Description]
		,0 [Metric_Value]
		,Holiday_Hours [Amt]--hours amount per location 
		,10 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'
UNION 
SELECT	Location_No_and_Description
		,'Holiday Worked Paid Hours' [Description]
		,0 [Metric_Value]
		,Holiday_WorkedS [Amt]--hours amount per location 
		,11 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'
UNION 
SELECT	Location_No_and_Description
		,'Jury Duty Hours' [Description]
		,0 [Metric_Value]
		,JURYS [Amt]--hours amount per location 
		,12 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'
UNION 
SELECT	Location_No_and_Description
		,'Bad Weather Day Hours' [Description]
		,0 [Metric_Value]
		,BAD_WEATHERS [Amt]--hours amount per location 
		,13 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'

UNION 
SELECT	Location_No_and_Description
		,'Non-Worked Paid Time Hours' [Description]
		,0 [Metric_Value]
		,NON_WRKS [Amt]--hours amount per location 
		,14 [Seq]
		,'Decimal' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'	
UNION 
SELECT	Location_No_and_Description
		,'Total Labor Hours' [Description]
		,0 [Metric_Value]
		,[Labor_Hrs] [Amt]--labor hours amount 
		,15 'Seq'--sequence rows show up in table 
		,'Decimal' [Value_Format] --format of the row            
		,ISNULL([Sales], 0) AS Sales--null
		,ISNULL([Sales_LY], 0) AS Sales_LY--null
		,ISNULL(Labor_Exp, 0) AS Labor_Exp--null
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY--null
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs--null
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY--null
		,ISNULL(Total_Donors, 0) AS Total_Donors--null
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '986' 
UNION 
SELECT	Location_No_and_Description
		,Metric [Description] -- Labor Hour/Donor
		,coalesce([Metric_Value],0)[Metric_Value]
		,[Labor_Hrs_Per_Donor][Amt]
		,16 'Seq'  
		,'Decimal' [Value_Format]              
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '986' 
/*-----------------------------------------[Sales_Per_Labor_Hr]-----------------------------------*/
UNION 
SELECT	Location_No_and_Description
		,Metric [Description] --Sales Per Labor Hour
		,coalesce([Metric_Value],0)[Metric_Value]
		,[Sales_Per_Labor_Hr] [Amt]
		,17 [Seq]
		,'Money' [Value_Format]            
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN
		#tempMetrics020 ON Metric_AC = '988'
UNION 
SELECT	Location_No_and_Description
		,'Labor Hrs LY' [Description]
		,0 [Metric_Value]
		,[Labor_Hrs_LY] [Amt]
		,18 [Seq] 
		,'Decimal' [Value_Format]             
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN
		#tempMetrics020 ON Metric_AC = '988'		  
UNION 
SELECT	Location_No_and_Description
		,'Sales Per Labor Hr LY' [Description]
		,coalesce([Metric_Value],0)[Metric_Value]
		,[Sales_Per_Labor_Hr_LY] [Amt] 
		,19 [Seq]
		,'Money' [Value_Format]             
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '988'		    
UNION 
SELECT	Location_No_and_Description
		,'Pct Chg Sls Per Lbr Hr LY' [Description] 
		,0 [Metric_Value]
		,[Pct_Chg_Sls_Per_Lbr_Hr_LY][Amt]
		,20 [Seq]
		,'Pct' [Value_Format]             
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '988'		    
/*-----------------------------------------[P/R Exp as % of Sales]-----------------------------------*/
UNION 
SELECT	Location_No_and_Description
		,'Actual Labor Exp % to Sales' [Description] -- Payroll %
		,coalesce([Metric_Value],0)[Metric_Value]
		,[PR_Exp_As_Pct_Sales] [Amt]
		,21 [Seq]
		,'Pct' [Value_Format]      
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN
		#tempMetrics020 ON Metric_AC = '987'  
UNION 
SELECT	Location_No_and_Description
		,'Labor Exp to Budget Variance' [Description]
		,0 [Metric_Value]
		,LaborBudgetVarianceCalc [Amt]--labor hours amount 
		,18 'Seq'--sequence rows show up in table 
		,'Money' [Value_Format] --format of the row            
		,ISNULL([Sales], 0) AS Sales--null
		,ISNULL([Sales_LY], 0) AS Sales_LY--null
		,ISNULL(Labor_Exp, 0) AS Labor_Exp--null
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY--null
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs--null
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY--null
		,ISNULL(Total_Donors, 0) AS Total_Donors--null
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '986' 	
UNION 
SELECT	Location_No_and_Description
		,'Labor Budget %' [Description]
		,0 [Metric_Value]
		, LaborBudgetPercent [Amt]--labor hours amount 
		,22 'Seq'--sequence rows show up in table 
		,'Pct' [Value_Format] --format of the row            
		,ISNULL([Sales], 0) AS Sales--null
		,ISNULL([Sales_LY], 0) AS Sales_LY--null
		,ISNULL(Labor_Exp, 0) AS Labor_Exp--null
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY--null
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs--null
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY--null
		,ISNULL(Total_Donors, 0) AS Total_Donors--null
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '000'       
UNION 
SELECT	Location_No_and_Description
		,'Labor Budget Calc' [Description]
		,0 [Metric_Value]
		, LaborBudgetCalc [Amt]--labor hours amount 
		,23 'Seq'--sequence rows show up in table 
		,'Decimal' [Value_Format] --format of the row            
		,ISNULL([Sales], 0) AS Sales--null
		,ISNULL([Sales_LY], 0) AS Sales_LY--null
		,ISNULL(Labor_Exp, 0) AS Labor_Exp--null
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY--null
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs--null
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY--null
		,ISNULL(Total_Donors, 0) AS Total_Donors--null
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '986'   
UNION
SELECT	Location_No_and_Description
		,'PR Exp As Pct Sales LY' [Description]
		,coalesce([Metric_Value],0)[Metric_Value]
		,[PR_Exp_As_Pct_Sales_LY]   [Amt]  
		,24 [Seq] 
		,'Pct' [Value_Format]          
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN
		#tempMetrics020 ON Metric_AC = '987' 
UNION 
SELECT	Location_No_and_Description
		,'Labor Exp LY' [Description]
		,0 [Metric_Value]
		,Labor_Exp_LY  [Amt]
		,25 [Seq]
		,'Money' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '987'  	
UNION
SELECT	Location_No_and_Description
		,'UltiPro Labor Expense' [Description]
		,0 [Metric_Value]
  		,Labor_Exp [Amt]
  		,26 [Seq] 
		,'Money' [Value_Format]  	  
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN
		#tempMetrics020 ON Metric_AC = '987'	
UNION
SELECT	Location_No_and_Description
		,'Contract Labor Expense' [Description]
		,coalesce([Metric_Value],0)[Metric_Value]
		,finalAmount   [Amt]  
		,27 [Seq] 
		,'Pct' [Value_Format]          
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN
		#tempMetrics020 ON Metric_AC = '987' 
UNION 
SELECT	Location_No_and_Description
		,'Total Labor Expense' [Description]
		,0 [Metric_Value]
		,runningTotal  [Amt]
		,28 [Seq]
		,'Money' [Value_Format]
		,ISNULL([Sales], 0) AS Sales
		,ISNULL([Sales_LY], 0) AS Sales_LY
		,ISNULL(Labor_Exp, 0) AS Labor_Exp
		,ISNULL(Labor_Exp_LY, 0) AS Labor_Exp_LY
		,ISNULL([Labor_Hrs], 0) AS Labor_Hrs
		,ISNULL([Labor_Hrs_LY], 0) AS Labor_Hrs_LY
		,ISNULL(Total_Donors, 0) AS Total_Donors
		,Budgets
		,finalBudget
FROM	#tempLabor_KPIs_010 LEFT JOIN 
		#tempMetrics020 ON Metric_AC = '987'  		
ORDER BY seq  --order the char