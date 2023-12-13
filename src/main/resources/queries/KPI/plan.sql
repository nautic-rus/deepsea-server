select *, (select login from users where plan.user_id= users.id), (select department  from users where plan.user_id= users.id)
from plan
