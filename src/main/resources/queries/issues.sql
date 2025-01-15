select
    *,
    (select closing_status from issue_types where i.issue_type like '%' || issue_types.type_name || '%' limit 1) as closing_status,
    (select stage_date from issue_stages istages where istages.stage_name = i.period and istages.issue_type = i.issue_type and id_project in (select id from issue_projects ip where ip.name = i.project) limit 1) as stage_date
from
    issue i
LEFT JOIN (select task_id, sum(amount) as actual_man_hours from issue_man_hours
    group by issue_man_hours.task_id) imh on i.id = imh.task_id
where
    removed = 0
