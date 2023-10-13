select department,
	count(CASE WHEN reason_of_changes = '1' THEN 1 ELSE NULL END) "Изменение контрагентских данных",
	count(CASE WHEN reason_of_changes = '2' THEN 1 ELSE NULL END) "Требование верфи",
	count(CASE WHEN reason_of_changes = '3' THEN 1 ELSE NULL END) as "Ошибка проектанта",
	count(CASE WHEN reason_of_changes = '4' THEN 1 ELSE NULL END) as "Требование заказчика",
	count(CASE WHEN reason_of_changes = '5' THEN 1 ELSE NULL END) as "Корректировка ПДСП",
	count(*) as "Total"
from issue
where project = 'NR002' and issue_type = 'CORRECTION' and removed = 0 and department in ('General','Devices','Hull','System','Electric','Accommodation')
group by department
union all
select null,
	count(CASE WHEN reason_of_changes = '1' THEN 1 ELSE NULL END) "Изменение контрагентских данных",
	count(CASE WHEN reason_of_changes = '2' THEN 1 ELSE NULL END) "Требование верфи",
	count(CASE WHEN reason_of_changes = '3' THEN 1 ELSE NULL END) as "Ошибка проектанта",
	count(CASE WHEN reason_of_changes = '4' THEN 1 ELSE NULL END) as "Требование заказчика",
	count(CASE WHEN reason_of_changes = '5' THEN 1 ELSE NULL END) as "Корректировка ПДСП",
	count(*) as "Total"
from issue
where project = 'NR002' and issue_type = 'CORRECTION' and removed = 0 and department in ('General','Devices','Hull','System','Electric','Accommodation');