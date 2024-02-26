select *
from issue_messages 
where issue_messages."date" in (select max("date") from issue_messages where prefix = 'answer' group by issue_id)