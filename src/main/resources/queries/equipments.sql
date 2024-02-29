SELECT
    eq.id, eq.sfi, eq.name, issue_departments.name as department, us.name as respons_name, us.surname as respons_surname, CASE WHEN ef.id is null THEN 0 ELSE 1 END as ITT
FROM
    equipments eq
        left join users us ON  eq.responsible_id = us.id
        left join issue_departments ON eq.department_id = issue_departments.id
        left join equipments_files ef on eq.id = ef.equ_id
