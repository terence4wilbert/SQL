ALTER TABLE [Empact-12-3-15].[dbo].[DivisionMaster] DISABLE TRIGGER ALL 

UPDATE [Empact-12-3-15].[dbo].[DivisionMaster] 
   SET User_Defined_1  = SF.OpportunityID
   FROM [Empact-12-3-15].[dbo].[DivisionMaster] DM INNER JOIN  [Empact-12-3-15].[dbo].[SFReport] SF ON DM.Division_ID = SF.LegacyDivisionId
   Where DM.Division_ID = SF.LegacyDivisionId