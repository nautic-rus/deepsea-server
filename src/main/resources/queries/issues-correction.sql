select issue.id, issue.status, issue.doc_number,
       count(case when issue_combined.issue_first = issue.id and i2.issue_type = 'CORRECTION' and i2.status not in ('Delivered') and i2.removed = 0 then  1 end),
       max (i2.due_date) as max_due_date
from issue
         left join issue_combined on issue.id= issue_combined.issue_first
         left join issue as i2 on i2.id= issue_combined.issue_second
where issue.issue_type in ('RKD', 'PDSP','PSD','ED','ITT') and issue.removed = 0
group by issue.id