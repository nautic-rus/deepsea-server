select department as "Department", 
sum(case when issue_type = 'RKD' then hours_amount else 0 end) as "RKD",
sum(case when issue_type = 'PDSP' then hours_amount else 0 end) as "PDSP",
sum(case when issue_type = 'CORRECTION' then hours_amount else 0 end) as "CORRECTION",
sum(case when issue_type = 'OTHER' then hours_amount else 0 end) as "OTHER",
sum(case when issue_type = 'QNA' then hours_amount else 0 end) as "QNA"
from plan 
join issue on task_id = issue.id 
where project = 'NR002'and removed = 0 and department in ('General','Devices','Hull','System','Electric','Accommodation')
group by department
union all 
select null, 
sum(case when issue_type = 'RKD' then hours_amount else 0 end) as "RKD",
sum(case when issue_type = 'PDSP' then hours_amount else 0 end) as "PDSP",
sum(case when issue_type = 'CORRECTION' then hours_amount else 0 end) as "CORRECTION",
sum(case when issue_type = 'OTHER' then hours_amount else 0 end) as "OTHER",
sum(case when issue_type = 'QNA' then hours_amount else 0 end) as "QNA"
from plan 
join issue on task_id = issue.id 
where project = 'NR002'and removed = 0 and department in ('General','Devices','Hull','System','Electric','Accommodation');


