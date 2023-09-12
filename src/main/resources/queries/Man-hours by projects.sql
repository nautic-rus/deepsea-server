select department as "Department", 
sum(case when project = 'NR002' then hours_amount else 0 end) as "NR002",
sum(case when project = 'NR004' then hours_amount else 0 end) as "NR004",
sum(case when project = '170701' then hours_amount else 0 end) as "170701",
sum(case when project = '03070-CRABBER' then hours_amount else 0 end) as "03070-CRABBER",
sum(case when project = '03095-ANDROMEDA' then hours_amount else 0 end) as "03095-ANDROMEDA",
sum(case when project = '01701-LEV' then hours_amount else 0 end) as "01701-LEV",
sum(case when project = '01701-ORION' then hours_amount else 0 end) as "01701-ORION",
sum(case when project = '03095-КАСТОР' then hours_amount else 0 end) as "03095-КАСТОР",
sum(hours_amount) as "Total"
from plan 
join issue on task_id = issue.id 
where removed = 0 and department in ('General','Devices','Hull','System','Electric','Accommodation')
group by department
union all 
select null, 
sum(case when project = 'NR002' then hours_amount else 0 end) as "NR002",
sum(case when project = 'NR004' then hours_amount else 0 end) as "NR004",
sum(case when project = '170701' then hours_amount else 0 end) as "170701",
sum(case when project = '03070-CRABBER' then hours_amount else 0 end) as "03070-CRABBER",
sum(case when project = '03095-ANDROMEDA' then hours_amount else 0 end) as "03095-ANDROMEDA",
sum(case when project = '01701-LEV' then hours_amount else 0 end) as "01701-LEV",
sum(case when project = '01701-ORION' then hours_amount else 0 end) as "01701-ORION",
sum(case when project = '03095-КАСТОР' then hours_amount else 0 end) as "03095-КАСТОР",
sum(hours_amount) as "Total"
from plan 
join issue on task_id = issue.id 
where removed = 0 and department in ('General','Devices','Hull','System','Electric','Accommodation');
