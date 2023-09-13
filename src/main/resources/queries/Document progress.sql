select department,
	count(*) as "All",
	count(CASE WHEN status = 'New' THEN 1 ELSE NULL END) "New",
	count(CASE WHEN status = 'Assigned' THEN 1 ELSE NULL END) "Assigne",
	count(CASE WHEN status = 'Pause' THEN 1 ELSE NULL END) as "Pause",
	count(CASE WHEN status = 'In Work' THEN 1 when status = 'In Rework' then 1 END) as "In Work\In Rework",
	count(CASE WHEN status = 'On Check' THEN 1 ELSE NULL END) as "On Check",
	count(CASE WHEN status = 'Hold' THEN 1 ELSE NULL END) as "Hold",
	count(CASE WHEN status = 'Ready to Delivery' THEN 1 ELSE null END) as "Ready to Delivery",
	count(CASE WHEN status = 'Delivered' THEN 1 when status = 'Closed' then 1 END) as "Delivered/Closed",
	round((cast ((count(CASE WHEN status = 'Delivered' THEN 1 when status = 'Closed' then 1 END)) as real)/cast((count(*)) as real)):: decimal, 2)*100 as "%"
from issue
where project = 'NR002' and issue_type = 'RKD' and removed = 0 and department in ('General','Devices','Hull','System','Electric','Accommodation')
group by department
union all
select null,
	count(*) as "All",
	count(CASE WHEN status = 'New' THEN 1 ELSE NULL END) "New",
	count(CASE WHEN status = 'Assigned' THEN 1 ELSE NULL END) "Assigne",
	count(CASE WHEN status = 'Pause' THEN 1 ELSE NULL END) as "Pause",
	count(CASE WHEN status = 'In Work' THEN 1 when status = 'In Rework' then 1 END) as "In Work\In Rework",
	count(CASE WHEN status = 'On Check' THEN 1 ELSE NULL END) as "On Check",
	count(CASE WHEN status = 'Hold' THEN 1 ELSE NULL END) as "Hold",
	count(CASE WHEN status = 'Ready to Delivery' THEN 1 ELSE null END) as "Ready to Delivery",
	count(CASE WHEN status = 'Delivered' THEN 1 when status = 'Closed' then 1 END) as "Delivered\Closed",
	round((cast ((count(CASE WHEN status = 'Delivered' THEN 1 when status = 'Closed' then 1 END)) as real)/cast((count(*)) as real)):: decimal, 2)*100 as "%"
from issue
where project = 'NR002' and issue_type = 'RKD' and removed = 0 and department in ('General','Devices','Hull','System','Electric','Accommodation');

