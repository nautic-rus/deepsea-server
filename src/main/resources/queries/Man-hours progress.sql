select department, sum (plan_hours) as "Plan", sum (hours_amount) as "Actual",
round((cast(sum (hours_amount) as real)/cast(sum (plan_hours) as real)):: decimal, 2)*100 as "%"
from issue 
inner join plan on issue.id = task_id 
where project = 'NR002' and issue_type = 'RKD' and removed ='0'
group by department
union all 
select null, sum (plan_hours) as "Plan", sum (hours_amount) as "Actual",
round((cast(sum (hours_amount) as real)/cast(sum (plan_hours) as real)):: decimal, 2)*100 as "%"
from issue 
inner join plan on issue.id = task_id 
where project = 'NR002' and issue_type = 'RKD' and removed ='0'
