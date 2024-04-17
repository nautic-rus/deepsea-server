SELECT
    eq.id, eq.sfi, eq.name, eq.descriptions, issue_departments.name as department, eq.comment as comment, eq.responsible_id, us.name as respons_name, us.surname as respons_surname,
    (select count(*) from equipments_files eqf where eqf.equ_id = eq.id) as ITT,
    issue_projects.name as project_name,
    eq.sfi_unit as sfi_unit,
    eq.parent_id as parent_id
FROM
    equipments eq
LEFT JOIN users us ON  eq.responsible_id = us.id
LEFT JOIN issue_departments ON eq.department_id = issue_departments.id
LEFT JOIN issue_projects ON eq.project_id = issue_projects.id
WHERE eq.removed = 0
