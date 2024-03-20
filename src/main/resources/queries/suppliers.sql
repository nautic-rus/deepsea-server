SELECT
    s.id as suppliers_id, s.user_id as user_id, s.equ_id as equip_id, s.name as equip_name, s.description equip_desc, s.comment as equip_comment, s.manufacturer as equip_manufacturer, ss.name as status, s.approvement, s.last_update
FROM
    suppliers s
LEFT JOIN suppliers_status ss ON s.status_id = ss.id
WHERE s.removed = 0
