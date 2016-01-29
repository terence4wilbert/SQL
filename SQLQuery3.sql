declare @BegDate datetime
declare @EndDate datetime

set @BegDate = GETDATE()
set @EndDate = GETDATE()

--Declaration of variables
declare @LastBusinessDay varchar(9),   -- Last Business Day
            @PrevBusinessDay varchar(9)   -- Same Business Day last year
--Initalization of variables
set @LastBusinessDay = convert(varchar(9),dateadd(wk, -52, @BegDate) ,112)
set @PrevBusinessDay = CONVERT(varchar(9),dateadd(wk, -52,@EndDate),112)


-- To compare the same day with the same business day in the year 
-- Monday  Jan. 11 2010  ---> Monday Jan 12, 2009 (Rather than Sun Jan 11, 2009)
declare @LBDName varchar(9), @PBDName varchar(9), @LBDName1 varchar(9),@PBDName1 varchar(9),@LBDName2 varchar(9),@PBDName2 varchar(9)
set @LBDName = DATENAME(DW, @LastBusinessDay)
set @LBDName1 = DATENAME(yy, @LastBusinessDay)
set @LBDName2 = DATENAME(mm, @LastBusinessDay)
set @PBDName = DATENAME(DW, @PrevBusinessDay)
set @PBDName1 = DATENAME(yy, @PrevBusinessDay)
set @PBDName2 = DATENAME(mm, @PrevBusinessDay)
select  GETDATE(),@LBDName,@LBDName2, @LBDName1 as [This Year] union select  GETDATE(),@PBDName,@PBDName2, @PBDName1 as [Last Year]




--set @BegDateLY = CONVERT(varchar(9),dateadd(wk, -52, @BegDate),112) --Calculated Last Year Beg Date
--set @EndDateLY = CONVERT(varchar(9),dateadd(wk, -52, @EndDate),112) --Calculated Last Year End Date 
