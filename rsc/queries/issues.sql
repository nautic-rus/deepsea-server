select
    *
from
    issue i, issue_stages ist
where
    removed = 0 and
    issue_type = 'QNA' and
    ist.stage_name = i.period and ist.id_project = (select id from issue_projects ip where ip.name = i.project)
