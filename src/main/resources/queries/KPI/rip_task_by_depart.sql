select issue.department , 
-- 2592000000 ms - 30 days
-- 7776000000 ms - 90 days
-- 15552000000 ms - 180 days
--AssigneTo more them 90 days
count(case when issue.last_update < 1703230161479-7776000000 and  issue.removed = 0 and issue.status = 'AssignedTo' then 1 end) as "AssignedTo",
--In work more them 90 days 
count(case when issue.last_update < 1703230161479-7776000000 and  issue.removed = 0 and issue.status = 'In Work' then 1 end) as "In work",
--Sent RS more them 180 days 
count(case when issue.last_update < 1703230161479-15552000000 and  issue.removed = 0 and issue.status = 'Send to RS' then 1 end) as "Send to RS",
--Sent OWNR more them 180 days
count(case when issue.last_update < 1703230161479-15552000000 and  issue.removed = 0 and issue.status = 'Send to Owner' then 1 end) as "Send to Owner",
--Ready Delivery more them 30 days
count(case when issue.last_update < 1703230161479-2592000000 and  issue.removed = 0 and issue.status = 'Ready to Delivery' then 1 end) as "Ready to Delivery",
--Check more them 30 days
count(case when issue.last_update < 1703230161479-2592000000 and  issue.removed = 0 and issue.status = 'Check' then 1 end) as "Check",
-- Summ 
count(case when issue.last_update < 1703230161479-7776000000 and  issue.removed = 0 and issue.status = 'AssignedTo' then 1 end) + 
count(case when issue.last_update < 1703230161479-7776000000 and  issue.removed = 0 and issue.status = 'In Work' then 1 end) +
count(case when issue.last_update < 1703230161479-15552000000 and  issue.removed = 0 and issue.status = 'Send to RS' then 1 end)+
count(case when issue.last_update < 1703230161479-15552000000 and  issue.removed = 0 and issue.status = 'Send to Owner' then 1 end)+
count(case when issue.last_update < 1703230161479-2592000000 and  issue.removed = 0 and issue.status = 'Ready to Delivery' then 1 end)+
count(case when issue.last_update < 1703230161479-2592000000 and  issue.removed = 0 and issue.status = 'Check' then 1 end) as "Summ",
-- KPI %
(case when (count(case when issue.last_update < 1703230161479-7776000000 and  issue.removed = 0 and issue.status = 'AssignedTo' then 1 end) + 
			count(case when issue.last_update < 1703230161479-7776000000 and  issue.removed = 0 and issue.status = 'In Work' then 1 end) +
			count(case when issue.last_update < 1703230161479-15552000000 and  issue.removed = 0 and issue.status = 'Send to RS' then 1 end)+
			count(case when issue.last_update < 1703230161479-15552000000 and  issue.removed = 0 and issue.status = 'Send to Owner' then 1 end)+
			count(case when issue.last_update < 1703230161479-2592000000 and  issue.removed = 0 and issue.status = 'Ready to Delivery' then 1 end)+
			count(case when issue.last_update < 1703230161479-2592000000 and  issue.removed = 0 and issue.status = 'Check' then 1 end))> 0 then 0 else 25 end) as "%"
from issue
join users on issue.assigned_to = users.login
where issue.removed = 0 and users.removed = 0
group by issue.department 
