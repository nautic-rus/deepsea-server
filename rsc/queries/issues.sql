select
    *,
    (select closing_status from issue_types where type_name = i.issue_type) as closing_status,
    (select stage_date from issue_stages istages where istages.stage_name = i.period and id_project in (select id from issue_projects ip where ip.name = i.project)) as stage_date
from
    issue i
where
    removed = 0
