select to_char(to_timestamp(date_start/1000), 'dd-mm-yyyy, DY') as "Date", issue.id, issue_type, issue_name, doc_number, hours_amount
from plan
join users on plan.user_id = users.id
join issue on plan.task_id = issue.id 
where login = 'mamyshev' and date_start >= 1693515600000 and date_start <= 1694466000000
order by date_start;
