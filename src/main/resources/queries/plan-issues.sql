select
    id, issue_name, doc_number, plan_hours, status, issue_type, period, assigned_to, project, department,
    (select closing_status from issue_types where type_name = i.issue_type limit 1) as closing_status,
    (select stage_date from issue_stages istages where istages.stage_name = i.period and istages.issue_type = i.issue_type and id_project in (select id from issue_projects ip where ip.name = i.project) limit 1) as stage_date
from
    issue i
where
    removed = 0
