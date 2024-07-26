SELECT stock_code, materials.name, materials.description, unit, materials.weight, statement_id, directory_id, materials.user_id, default_label, materials.last_update, note, materials.manufacturer, coef,
       sup_mat_relations.supplier_id as "supplier_id", suppliers_name.name as "supplier_name"
from materials
         LEFT JOIN sup_mat_relations ON materials.id = sup_mat_relations.materials_id
         LEFT JOIN suppliers ON sup_mat_relations.supplier_id = suppliers.id
         LEFT JOIN suppliers_name ON suppliers.suppliers_name_id = suppliers_name.id