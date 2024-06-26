SELECT sup_mat_relations.materials_id,
       materials.name,
       materials.stock_code,
       issue.doc_number,
       issue_esp.task_id as issue_id,
       suppliers.equ_id,
       issue_departments.name as dep_name,
       issue_projects.foran
FROM sup_mat_relations
         LEFT JOIN issue_esp ON sup_mat_relations.materials_id = issue_esp.materials_id
         LEFT JOIN materials ON sup_mat_relations.materials_id = materials.id
         LEFT JOIN issue ON issue_esp.task_id = issue.id
         LEFT JOIN suppliers ON suppliers.id = sup_mat_relations.supplier_id
         LEFT JOIN equipments ON suppliers.equ_id = equipments.id
         LEFT JOIN issue_departments ON equipments.department_id = issue_departments.id
         LEFT JOIN issue_projects ON equipments.project_id = issue_projects.id
WHERE sup_mat_relations.supplier_id = &supId;
