select
    *,
    (select closing_status from issue_types where i.issue_type like '%' || issue_types.type_name || '%' limit 1) as closing_status,
    (select stage_date from issue_stages istages where istages.stage_name = i.period and istages.issue_type = i.issue_type and id_project in (select id from issue_projects ip where ip.name = i.project) limit 1) as stage_date
from
    issue i
where
    removed = 0
