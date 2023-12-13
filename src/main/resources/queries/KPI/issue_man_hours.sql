select *, (select login from users where issue_man_hours.user_id= users.id),(select department  from users where issue_man_hours.user_id= users.id)
from issue_man_hours