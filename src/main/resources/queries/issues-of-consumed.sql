select
    i.id,
    i.issue_type,
    i.doc_number,
    i.issue_name,
    i.project,
    i.department,
    i.status,
    i.contract,
    i.start_date,
    i.due_date,
    i.plan_hours,
    i.assigned_to,
    i.period,
    i.revision,
    (select closing_status from issue_types where i.issue_type like '%' || issue_types.type_name || '%' limit 1) as closing_status,
    (select stage_date from issue_stages istages where istages.stage_name = i.period and istages.issue_type = i.issue_type and id_project in (select id from issue_projects ip where ip.name = i.project) limit 1) as stage_date,
    i.removed
from
    issue i
where
    i.id in (select distinct (task_id) from issue_man_hours)
