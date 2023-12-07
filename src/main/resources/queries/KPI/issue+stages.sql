select issue.id, issue.status, issue.issue_name, issue_stages.stage_date, issue.department
from issue
join issue_projects on issue.project = issue_projects."name"
join issue_stages on issue_stages.id_project = issue_projects.id and issue."period" = issue_stages.stage_name and issue.issue_type = issue_stages.issue_type 