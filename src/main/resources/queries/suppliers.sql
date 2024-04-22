SELECT
    s.id as suppliers_id,
    s.user_id as user_id,
    s.equ_id as equip_id,
    sn.name as sup_name,
    sn.id as sup_id,
    s.description equip_desc,
    s.comment as equip_comment,
    s.manufacturer as equip_manufacturer,
    ss.name as status,
    s.approvement,
    s.last_update,
    s.model,
    s.ele_param,
    s.mech_param,
    s.weight
FROM
    suppliers s
LEFT JOIN suppliers_status ss ON s.status_id = ss.id
LEFT JOIN suppliers_name sn ON sn.id = s.suppliers_name_id
WHERE s.removed = 0
