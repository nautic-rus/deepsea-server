select users.id,"name", surname, department, sum (plan.hours_amount) as "Man-hours"
from users
join plan on plan.user_id = users.id
where plan.date_start > 1693515600000 and plan.date_finish < 1696021200000 and users."group" in ('Engineers', 'Hull Engineers') and plan.consumed = 1
group by users.id
order by users.department ;