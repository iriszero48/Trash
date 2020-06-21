select products.id,products.name,isbn,company_id,companies.name as company_name, price from products ,companies where products.company_id = companies.id;
