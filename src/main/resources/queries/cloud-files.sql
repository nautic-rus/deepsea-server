select
    fc.fileid,
    fc.name,
    fc.mtime,
    fc.path,
    (select oa.user from oc_activity oa where object_id = fc.fileid and subject = 'created_self' limit 1) as user
from
    oc_filecache fc
where path  like '%&filter%' and path not like '%/trash/%' and path not like '%/arh/%'
