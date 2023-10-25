select date_start, issue.id, issue_type, issue_name, doc_number, hours_amount
from plan
join users on plan.user_id = users.id
join issue on plan.task_id = issue.id 
where date_start >= &dateStart and date_start <= &dateFinish
and login in (&users)
