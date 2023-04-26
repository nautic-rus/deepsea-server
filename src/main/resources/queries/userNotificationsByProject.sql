select email from users
where doc_notification = 1 and id in
    (select user_id from users_visibility_projects where project_id in
        (select p.id from issue_projects p where p.name = '&project'))
