select
    *,
    (select status from issue i where p.task_id = i.id),
    (select closing_status from issue_types where (select issue_type from issue i where p.task_id = i.id) like '%' || issue_types.type_name || '%' limit 1) as closing_status
from plan p
