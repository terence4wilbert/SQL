--Today
SELECT cast(convert(varchar(8),getdate(),1) as datetime) union
--Yesterday
SELECT cast(convert(varchar(8),getdate()-1,1) as datetime) union
--LAST YEAR
SELECT cast(convert(varchar(8),(dateadd(year,-1,getdate()-1)),1) as datetime) 

--Leap Year
--Today
SELECT DATEADD(day,DATEDIFF(day, 0, GETDATE()),0) union
--Yesterday
SELECT DATEADD(day,DATEDIFF(day, 0, GETDATE()),-1)union
--LAST YEAR
SELECT DATEADD(YEAR,-1,(DATEADD(day,DATEDIFF(day, 0, GETDATE()),-2)))