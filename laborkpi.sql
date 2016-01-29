--/*
--Test Declarations
declare @BegDate datetime
declare @EndDate datetime
declare @Locations varchar (max)
declare @BudgetID char (15)

--Test Criteria
set @begdate = '10/01/2015'
set @enddate = '10/28/2015'
set @locations = '1258'
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
    
IF object_id('tempdb..#tempUltiproExpensesGRANDFATHER') IS NOT NULL
    DROP TABLE #tempUltiproExpensesGRANDFATHER
IF object_id('tempdb..#tempLabor_KPIs_010') IS NOT NULL
    DROP TABLE #tempLabor_KPIs_010


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
INTO	#tempLaborHoursByLocation010      
FROM	#payroll_import_calculate ap JOIN
		#tempTerritoriesAndStores ts ON ts.Locn_No = ap.DepartmentNumber
WHERE	(cast([Date] as datetime) between dateadd(YYYY,-1,@BegDate) and @EndDate ) and  ap.[DepartmentNumber] in (@Locations) and ap.Locn_No = ts.Locn_No--compare location of 2 seperate temp tables to get hours by location 
GROUP BY [DepartmentNumber]
order by DepartmentNumber

INSERT INTO #tempLaborHoursByLocation010(Store, Labor_Hrs, Labor_Hrs_LY, Labor_Exp, Labor_Exp_LY)
SELECT	Locn_No, 0, 0, 0, 0
FROM	#tempTerritoriesAndStores
WHERE	Locn_No NOT IN (SELECT Store FROM #tempLaborHoursByLocation010)

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
      ,case when ([BUDGETAMT]) is null then 0 else ([BUDGETAMT])end [ACTNUMBR_2]
      ,[PERIODDT]
      ,[PERIODID] 
	   ,case when ([BUDGETAMT]) is null then 0 else ([BUDGETAMT])end Budgets
      ,[YEAR1]
      ,Locn_No
  into #tempbudget 
  FROM [GL00201],#tempTerritoriesAndStores
  where  Locn_No = [ACTNUMBR_2] and ACTNUMBR_2  in (@Locations)
  and [PERIODID] between month (@BegDate) and month(@EndDate)
  and ACTNUMBR_1 = '701'  and BUDGETID = 'BUDGET 2015'
  order by BUDGETID, ACTNUMBR_2
select * from #tempbudget
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
Select  Locn_No,[Company Code],[ACTNUMBR_2],[Store Location],[ACTNUMBR_1],[Org Level 3],[Employee Name],[Employee Number],[Earnings Code],[Hours],[Amount],[Pay Date],[Period Start Date] ,[Period End Date]
Into #tempCPTO
FROM [GWC].[dbo].[Payroll_Expenses] PE left join #tempTerritoriesAndStores ts on pe.ACTNUMBR_2 = ts.Locn_No
where [Period End Date] between @BegDate and @EndDate and ACTNUMBR_2 in (@Locations)
Select Locn_No ,case when sum([Amount]) IS null then 0 else sum([Amount]) end  [RETRO]
Into #tempUltiproExpensesRETRO
FROM #tempCPTO 
WHERE [ACTNUMBR_1] between '701' and '706' AND [Earnings Code] = 'RETRO'
group by Locn_No
Select Locn_No ,case when sum([Amount]) IS null then 0 else sum([Amount]) end [RETRO_HOLIDAY]
Into #tempUltiproExpensesRETRO_HOLIDAY
FROM #tempCPTO 
WHERE [ACTNUMBR_1] between '701' and '707' AND [Earnings Code] = 'REHOL'
group by Locn_No
Select Locn_No ,case when sum([Amount]) IS null then 0 else sum([Amount]) end  [REFFERAL]
Into #tempUltiproExpensesREFFERAL
FROM #tempCPTO 
WHERE [ACTNUMBR_1] between '701' and '708' AND [Earnings Code] = 'REFFE'
group by Locn_No
Select Locn_No ,case when sum([Amount]) IS null then 0 else sum([Amount]) end  [BONUS]
Into #tempUltiproExpensesBONUS
FROM #tempCPTO
WHERE [ACTNUMBR_1] between '701' and '709' AND [Earnings Code] = 'BON'
group by Locn_No
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
/*--------------------------------------------[Labor Query: Labor Hrs/Donor and Sales/Labor Hr and Textile/donor and textile/labor hr]--------------------------*/
SELECT GL1.ACTINDX as ACTINDX1,ACTNUMBR_1 As Account_Number,ACTNUMBR_2 as Locn_No,ACTNUMBR_3,DSCRIPTN,TRXDATE,DEBITAMT 
  into #tempGSG
  FROM [GWC].[dbo].[GL20000] GL1 left join [GWC].[dbo].[GL00100] GL2 on gl1.ACTINDX = gl2.ACTINDX
  where ACTNUMBR_1 = '607' and TRXDATE between @BegDate and @EndDate and ACTNUMBR_2 in (@Locations)
  order by ACTNUMBR_2, TRXDATE
Select ACTINDX1,Account_Number,Locn_No,sum(DEBITAMT) as finalAmount
into #tempGSG2
from #tempGSG
group by ACTINDX1, Account_Number,Locn_No
--select * from  #tempGSG2
SELECT	Store,Location_No_and_Description,finalAmount,case when finalAmount is null then labor_exp else (Labor_Exp + finalAmount) end runningTotal
INTO	#tempTotalLabor          
FROM	#tempTerritoriesAndStores tg left join #tempLaborHoursByLocation010 tl on tg.Locn_No = tl.Store  left join #tempGSG2 ts on tl.Store = ts.Locn_No
INSERT INTO #tempTotalLabor(Store, Location_No_and_Description, finalAmount, runningTotal)
SELECT	Locn_No, Location_No_and_Description, 0, 0
FROM	#tempTerritoriesAndStores
WHERE	Locn_No NOT IN (SELECT LEFT(Location_No_and_Description, 4) FROM #tempTotalLabor   WHERE Location_No_and_Description IS NOT NULL)
--select * from #tempTotalLabor
SELECT	tl.Store,Location_No_and_Description, case when RETRO is null then 0 else RETRO end [RETRO],case when RETRO_HOLIDAY Is null then 0 else RETRO_HOLIDAY end [RETRO_HOLIDAY],CASE when REFFERAL Is null then 0 else REFFERAL end [REFFERAL],CASE WHEN BONUS IS NULL THEN 0 ELSE BONUS END [BONUS],CASE WHEN ANNIV IS NULL THEN 0 ELSE ANNIV END [ANNIV],CASE WHEN CPTO IS NULL THEN 0 ELSE CPTO END [CPTO],CASE WHEN FINALLEAVE IS NULL THEN 0 ELSE FINALLEAVE END [FINALLEAVE],CASE WHEN GRANDFATHER IS NULL THEN 0 ELSE GRANDFATHER END [GRANDFATHER],
		(runningTotal + CASE WHEN ANNIV IS NULL THEN 0 ELSE ANNIV END  + CASE WHEN BONUS IS NULL THEN 0 ELSE BONUS END + CASE when REFFERAL Is null then 0 else REFFERAL end + case when RETRO_HOLIDAY Is null then 0 else RETRO_HOLIDAY end +  case when RETRO is null then 0 else RETRO end + CASE WHEN CPTO IS NULL THEN 0 ELSE CPTO END + CASE WHEN FINALLEAVE IS NULL THEN 0 ELSE FINALLEAVE END + CASE WHEN GRANDFATHER IS NULL THEN 0 ELSE GRANDFATHER END) [ULTIPRO_TOTAL] 
INTO	#tempTotaLabor2     
FROM	#tempTotalLabor tl left join #tempUltiproExpensesBONUS tb on tl.store = tb.Locn_No left join #tempUltiproExpensesREFFERAL tr on tl.Store = tr.Locn_No left join #tempUltiproExpensesRETRO uer on tl.Store = uer.Locn_No left join #tempUltiproExpensesRETRO_HOLIDAY UERH on tl.Store = UERH.Locn_No left join #tempUltiproExpensesANNIVERSITY an on  tl.Store = an.Locn_No left join #tempLaborHoursByLocation010 th on tl.store = th.Store left join #tempUltiproExpensesCPTO cp on tl.Store = cp.Locn_No left join #tempUltiproExpensesFINALLEAVE fl on fl.Locn_No = tl.Store LEFT JOIN #tempUltiproExpensesGRANDFATHER GF ON GF.Locn_No = TL.Store
INSERT INTO #tempTotaLabor2 (tl.Store, Location_No_and_Description, RETRO, RETRO_HOLIDAY, REFFERAL, BONUS,ANNIV, ULTIPRO_TOTAL)
SELECT	Locn_No, Location_No_and_Description, 0, 0, 0, 0,0,0
FROM	#tempTerritoriesAndStores
WHERE	Locn_No NOT IN (SELECT LEFT(Location_No_and_Description, 4) FROM #tempTotalLabor   WHERE Location_No_and_Description IS NOT NULL)
--select * from #tempTotaLabor2
SELECT	lh.Store,DL.Location_No_and_Description,[Labor_Hrs],[Labor_Hrs_LY],[Sales],[Sales_LY],Labor_Exp,Labor_Exp_LY,Total_Donors,Budgets,finalBudget,runningTotal, ULTIPRO_TOTAL
		,CASE WHEN [Total_Donors] IS NOT NULL AND [Total_Donors] <>0 THEN [Labor_Hrs]/[Total_Donors] ELSE 0 END [Labor_Hrs_Per_Donor]
		,CASE WHEN  [Labor_Hrs]  IS NOT NULL AND [Labor_Hrs]<> 0 THEN [Sales]/[Labor_Hrs] ELSE 0 END [Sales_Per_Labor_Hr]
		,case when [Labor_Hrs_LY] IS NOT NULL AND [Labor_Hrs_LY]<> 0then [Sales_LY]/[Labor_Hrs_LY] else 0 end [Sales_Per_Labor_Hr_LY] 
		,case when (case when [Labor_Hrs_LY] IS NOT NULL AND [Labor_Hrs_LY]<>0 then [Sales_LY]/[Labor_Hrs_LY] else 0 end)<>0 then case when [Labor_Hrs] <>0 then [Sales]/[Labor_Hrs] else 0 end/case when [Labor_Hrs_LY]<>0 then [Sales_LY]/[Labor_Hrs_LY] else 0 end else 0 end [Pct_Chg_Sls_Per_Lbr_Hr_LY]
		,case when [Sales]<>0 and [Sales] is not null then ULTIPRO_TOTAL/[Sales]   else 0 end [PR_Exp_As_Pct_Sales] 
		,case when labor_exp <> 0 and labor_exp is not null then (([Budgets]-ULTIPRO_TOTAL)) else 0 end [Payroll_Var] 
		,case when [Sales_LY]<>0  and [Sales_LY] is not null then Labor_Exp_LY/[Sales_LY]  else 0 end [PR_Exp_As_Pct_Sales_LY]    
		,CASE WHEN  finalBudget IS NOT NULL AND finalBudget <> 0 THEN (Budgets/finalBudget) ELSE 0 END [LaborBudgetPercent]
		,((CASE WHEN  finalBudget  IS NOT NULL AND finalBudget <> 0 THEN (Budgets/finalBudget) ELSE 0 END ) * Sales ) [LaborBudgetCalc]
		,case when ((CASE WHEN  finalBudget  IS NOT NULL AND finalBudget <> 0 THEN (Budgets/finalBudget) ELSE 0 END ) * Sales ) <> 0 and finalBudget IS not null then  (((ULTIPRO_TOTAL) - (((CASE WHEN  finalBudget  IS NOT NULL AND finalBudget <> 0 THEN (Budgets/finalBudget) ELSE 0 END ) * Sales ))))  else ULTIPRO_TOTAL  end [LaborBudgetVarianceCalc]
		--, ULTIPRO_TOTAL [LaborBudgetVarianceCalc]
INTO	#tempLabor_KPIs_010           
FROM	#tempbudget tb,#tempTotalDonorsByLocation DL  -- Donors by Loc
		left join #tempLaborHoursByLocation010   LH ON LH.Store = DL.Locn  -- Labor Hrs
		left join #tempRevenueByLocation010 ON Store = Rev_Locn left join #tempBudget06 tb6 on tb6.LocnNo = Rev_Locn left join #tempTotalLabor tg on tg.Store = DL.Locn left join #tempTotaLabor2 t2 on dl.Locn = t2.Store
where  tb.Locn_No = dl.Locn --****compare location to make sure numbers change as store location change for both sales transactions and total textile 
INSERT INTO #tempLabor_KPIs_010(Store, Location_No_and_Description, Labor_Hrs, Labor_Hrs_Per_Donor, Sales_Per_Labor_Hr, Labor_Hrs_LY, 
							Sales_Per_Labor_Hr_LY,[Pct_Chg_Sls_Per_Lbr_Hr_LY], Sales, Sales_LY, Labor_Exp, Labor_Exp_LY, Total_Donors,
							PR_Exp_As_Pct_Sales, Payroll_Var, PR_Exp_As_Pct_Sales_LY,Budgets,LaborBudgetPercent,LaborBudgetCalc,LaborBudgetVarianceCalc,finalBudget, runningTotal,ULTIPRO_TOTAL)
SELECT	Locn_No, Location_No_and_Description, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0,0,0,0,0
FROM	#tempTerritoriesAndStores
WHERE	Locn_No NOT IN (SELECT store FROM #tempLabor_KPIs_010)
--select * from #tempLabor_KPIs_010
 /*select statement for the final temp table to display after the unions*/  
SELECT	Location_No_and_Description
		,('Total'+ ' '+ Metric) [Description] -- Labor Hour/Donor
		,coalesce([Metric_Value],0)[Metric_Value]
		,[Labor_Hrs_Per_Donor][Amt]
		,1 'Seq'  
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
UNION 
SELECT	Location_No_and_Description
		,Metric [Description] --Sales Per Labor Hour
		,coalesce([Metric_Value],0)[Metric_Value]
		,[Sales_Per_Labor_Hr] [Amt]
		,2 [Seq]
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
		#tempMetrics020 ON Metric_AC = '988'		  
UNION 
SELECT	Location_No_and_Description
		,'Sales Per Labor Hr LY' [Description]
		,coalesce([Metric_Value],0)[Metric_Value]
		,[Sales_Per_Labor_Hr_LY] [Amt] 
		,4 [Seq]
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
		,5 [Seq]
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
UNION 
SELECT	Location_No_and_Description
		,'Total Labor Exp % to Sales' [Description] -- Payroll %
		,coalesce([Metric_Value],0)[Metric_Value]
		,[PR_Exp_As_Pct_Sales] [Amt]
		,6 [Seq]
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
		,'Total Labor Exp to Budget Variance' [Description]
		,0 [Metric_Value]
		,LaborBudgetVarianceCalc [Amt]--labor hours amount 
		,11 'Seq'--sequence rows show up in table 
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
		,7 'Seq'--sequence rows show up in table 
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
		,8 'Seq'--sequence rows show up in table 
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
		,'PR Exp As Pct Sales LY' [Description]
		,coalesce([Metric_Value],0)[Metric_Value]
		,[PR_Exp_As_Pct_Sales_LY]   [Amt]  
		,9 [Seq] 
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
		,10 [Seq]
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
ORDER BY seq  --order the chart by the sequence numbers 1-11
                      