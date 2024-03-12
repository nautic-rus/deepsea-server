SELECT
    eq.id, eq.sfi, eq.name, eq.descriptions, issue_departments.name as department, eq.comment as comment, eq.responsible_id, us.name as respons_name, us.surname as respons_surname, CASE WHEN ef.id is null THEN 0 ELSE 1 END as ITT,
    issue_projects.name as project_name
FROM
    equipments eq
LEFT JOIN users us ON  eq.responsible_id = us.id
LEFT JOIN issue_departments ON eq.department_id = issue_departments.id
LEFT JOIN equipments_files ef on eq.id = ef.equ_id
LEFT JOIN issue_projects ON eq.project_id = issue_projects.id
WHERE eq.removed = 0
