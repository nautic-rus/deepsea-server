SELECT
    s.id as suppliers_id, s.equ_id as equipm_id, s.description, ss.name as status
FROM suppliers s
         LEFT JOIN suppliers_status ss ON s.status_id = ss.id
