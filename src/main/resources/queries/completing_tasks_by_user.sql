select issue.assigned_to,count(issue.id) as "Plan",
	   					count(CASE WHEN issue.status  = 'Closed' THEN 1 when ISSUE.status = 'Delivered' then 1 END) as "Result"
from issue
join issue_projects on issue.project = issue_projects."name" 
join issue_stages on issue."period" = issue_stages.stage_name and issue_projects.id = issue_stages.id_project 
where stage_date > 1688158800000 and stage_date < 1696021200000 
group by issue.assigned_to ;