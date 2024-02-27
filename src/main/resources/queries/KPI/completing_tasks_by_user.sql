select issue.assigned_to,
count(CASE WHEN issue.status  = 'AssignedTo' THEN 1 when ISSUE.status = 'In Work' then 1 END) as "Result",
case when (count(CASE WHEN issue.status  = 'AssignedTo' THEN 1 when ISSUE.status = 'In Work' then 1 END)) > 0 then 0 else 25 end as "%"
from issue
where issue.due_date < 1703140998239 and removed = 0
group by issue.assigned_to;