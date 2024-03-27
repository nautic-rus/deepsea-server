SELECT sup_task_relations.id, i.id as issue_id, i.issue_type,i.issue_name, i.started_by, i.responsible, i.assigned_to, i.status, i.removed
FROM sup_task_relations
         LEFT JOIN  issue i ON sup_task_relations.task_id = i.id
WHERE sup_task_relations.suppliers_id = &supplier_id  and i.removed = 0
