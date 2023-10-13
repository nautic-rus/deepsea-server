select department,
	sum(CASE WHEN reason_of_changes = '1' THEN hours_amount else 0 END) "Изменение контрагентских данных",
	sum(CASE WHEN reason_of_changes = '2' THEN hours_amount else 0 END) "Требование верфи",
	sum(CASE WHEN reason_of_changes = '3' THEN hours_amount else 0 END) as "Ошибка проектанта",
	sum(CASE WHEN reason_of_changes = '4' THEN hours_amount else 0 END) as "Требование заказчика",
	sum(CASE WHEN reason_of_changes = '5' THEN hours_amount else 0 END) as "Корректировка ПДСП",
	sum(hours_amount) as "Total"
from issue
join plan on plan.task_id=issue.id
where project = 'NR002' and issue_type = 'CORRECTION' and removed = 0 and department in ('General','Devices','Hull','System','Electric','Accommodation')
group by department
union all
select null,
	sum(CASE WHEN reason_of_changes = '1' THEN hours_amount else 0 END) "Изменение контрагентских данных",
	sum(CASE WHEN reason_of_changes = '2' THEN hours_amount else 0 END) "Требование верфи",
	sum(CASE WHEN reason_of_changes = '3' THEN hours_amount else 0 END) as "Ошибка проектанта",
	sum(CASE WHEN reason_of_changes = '4' THEN hours_amount else 0 END) as "Требование заказчика",
	sum(CASE WHEN reason_of_changes = '5' THEN hours_amount else 0 END) as "Корректировка ПДСП",
	sum(hours_amount) as "Total"
from issue
join plan on plan.task_id=issue.id
where project = 'NR002' and issue_type = 'CORRECTION' and removed = 0 and department in ('General','Devices','Hull','System','Electric','Accommodation');