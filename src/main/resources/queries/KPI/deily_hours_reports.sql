select users.id, login, department, 
(select hours from production_calendar where year = 2024 and month = 01) as "Production_Plan", 
(select coalesce (sum(hours_amount), 0) from plan where users.id = plan.user_id and task_type != 0 and date_start >= 1704056400000 and date_finish <= 1706734800000) as "Absence",
(select coalesce (sum(amount), 0) from issue_man_hours where users.id = issue_man_hours.user_id and date_consumed >= 1704056400000 and date_consumed <= 1706734800000) as "Man-hours",
-- Total delta
(select coalesce (sum(hours_amount), 0) from plan where users.id = plan.user_id and task_type != 0 and date_start >= 1704056400000 and date_finish <= 1706734800000 )+
(select coalesce (sum(amount), 0)  from issue_man_hours where users.id = issue_man_hours.user_id and date_consumed >= 1704056400000 and date_consumed <= 1706734800000) as "Total Man-hours",
-- KPI%
(case when (select hours from production_calendar where year = 2024 and month = 01)<=
((select coalesce (sum(hours_amount), 0) from plan where users.id = plan.user_id and task_type != 0 and date_start >= 1704056400000 and date_finish <= 1706734800000 )+
(select coalesce (sum(amount), 0)  from issue_man_hours where users.id = issue_man_hours.user_id and date_consumed >= 1704056400000 and date_consumed <= 1706734800000))*1.05 then 25 else 0 end) as "KPI"
from users
where department in ('Accommodation', 'Hull', 'Electric', 'System', 'Devices', 'Design', 'General') and removed = 0 and login not in ('nabokov','kolesnik','sidorov','melnikov','dmitrieva','miklukhin','korobova','gerasimova','klestov','mamyshev')
order by users.department