select issue.assigned_to, 
--AssigneTo more them 90 days
count(case when issue.last_update < 1703230161479-7776000000 and  issue.removed = 0 and issue.status = 'AssignedTo' then 1 end) as "AssignedTo",
--In work more 90 days 
count(case when issue.last_update < 1703230161479-7776000000 and  issue.removed = 0 and issue.status = 'In Work' then 1 end) as "In work",
-- Sum 
count(case when issue.last_update < 1703230161479-7776000000 and  issue.removed = 0 and issue.status = 'AssignedTo' then 1 end) + 
count(case when issue.last_update < 1703230161479-7776000000 and  issue.removed = 0 and issue.status = 'In Work' then 1 end) as "Sum",
-- KPI %
(case when (count(case when issue.last_update < 1703230161479-7776000000 and  issue.removed = 0 and issue.status = 'AssignedTo' then 1 end) + 
count(case when issue.last_update < 1703230161479-7776000000 and  issue.removed = 0 and issue.status = 'In Work' then 1 end))> 0 then 0 else 25 end) as "%"
from issue
join users on issue.assigned_to = users.login
where issue.removed = 0 and users.removed = 0
group by issue.assigned_to
order by issue.assigned_to asc