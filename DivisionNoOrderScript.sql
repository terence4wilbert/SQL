SELECT DM.[Division_ID] [DIVISION ID],
		OD.Division_ID [ORDER MASTER DIVISION ID], 
		OD.Order_ID,
		DM.Division_Name,
		DM.CreateDatetime,
		DM.ModifyDatetime
FROM [EMPACT_001_PROD_PDI].[dbo].[OrderMaster] OD  RIGHT JOIN [EMPACT_001_PROD_PDI].[dbo].[DivisionMaster] DM ON OD.[Division_ID] = DM.[Division_ID]
--where OD.Division_ID IS NULL	
 GROUP BY DM.Division_ID, OD.Division_ID,  OD.Order_ID, DM.Division_Name, DM.CreateDatetime, DM.ModifyDatetime
 ORDER BY DM.Division_ID