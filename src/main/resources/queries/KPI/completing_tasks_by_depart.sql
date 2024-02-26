select issue.department,
count(CASE WHEN issue.status  not in ('Delivered', 'Joined', 'Send to RS', 'Send to Owner') and issue.issue_type in ('RKD', 'PDSP') THEN 1 END) as "Result",
case when (count(CASE WHEN issue.status  not in ('Delivered', 'Joined' ) and issue.issue_type in ('RKD', 'PDSP') THEN 1 END)) > 0 then 0 else 25 end as "%"
from issue
inner join issue_projects on issue.project = issue_projects."name" 
inner join issue_stages on issue_stages.id_project = issue_projects.id and issue."period" = issue_stages.stage_name and issue.issue_type = issue_stages.issue_type
where issue_stages.stage_date < 1706389200000 and issue_stages.stage_date > 0 and issue.removed = 0
group by issue.department ;