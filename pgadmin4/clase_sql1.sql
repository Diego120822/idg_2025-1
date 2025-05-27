/************ 

COMENTARIO LARGO

************/

-- SELECT
-- Simepre recrodar que es buena practica utilizar palabras resrvadas en MAYÚSCULA

SELECT *
FROM "Artist";

-- Seleccionar nombre y apellido de los clientes
SELECT "FirstName", "LastName"
FROM "Customer";

-- Seleccionar todos los clientes de Canada
SELECT *
FROM "Customer"
WHERE "Country" = 'Canada' -- Comillas simples para atributos
ORDER BY "LastName" ASC; 

-- Contar registros
SELECT COUNT(*)
FROM "Customer"
WHERE "Country" = 'Canada';

-- Número de Clientes 
SELECT "Country" AS pais, COUNT(*) AS n_clientes
FROM "Customer"
GROUP BY "Country"
ORDER BY n_clientes DESC --Ordenar
LIMIT 5; -- Acotar






