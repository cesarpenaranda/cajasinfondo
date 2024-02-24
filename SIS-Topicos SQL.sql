CREATE DATABASE  Holamundo; 

SELECT name FROM sys. databases; -- MUESTRA MIS DATABASES 

-- tipos de datos 
-- int: numeros enteros 
-- decimal(float): decimales
-- varchar: cadena de caracteres

-- indicamos que base de datos usamos 
USE Holamundo
GO
-- creamos table 
CREATE TABLE animales(
id INT IDENTITY(1,1),
tipo VARCHAR(255),
estado VARCHAR(255),
PRIMARY KEY (id)
);

-- insertar datos en la tabla 
INSERT INTO animales(tipo,estado) VALUES('chanchito','feliz');

-- para borrar la tabla
DROP TABLE animales

-- insertar datos en la tabla 
INSERT INTO animales(tipo,estado) VALUES('chanchito','feliz');
INSERT INTO animales(tipo,estado) VALUES('dragon','feo');
INSERT INTO animales(tipo,estado) VALUES('perro','feliz');

-- select
SELECT * FROM animales;
SELECT * FROM animales WHERE id =1;
SELECT * FROM animales WHERE estado = 'feliz';
SELECT * FROM animales WHERE estado = 'feliz' AND tipo='perro';

--actualizar registros
UPDATE animales SET estado='lindo' WHERE id='1';

-- eliminar registros 
DELETE FROM animales WHERE estado='feliz';

-- crear otra tabla
CREATE TABLE users(
id INT IDENTITY(1,1),
NAME VARCHAR(255) NOT NULL, 
edad INT NOT NULL, 
email VARCHAR(255) NOT NULL, 
PRIMARY KEY (id)
);

INSERT INTO users (NAME,edad,email) VALUES ('oscar','25','oscar@gmail.com')
INSERT INTO users (NAME,edad,email) VALUES ('adriana','27','adriana@gmail.com')
INSERT INTO users (NAME,edad,email) VALUES ('maria','30','maria@gmail.com')

SELECT * FROM users; 

SELECT TOP 1 * FROM users;

SELECT * FROM users WHERE edad>25;

SELECT * FROM users WHERE edad>= 25; 

SELECT * FROM users WHERE edad >25 AND email='adriana@gmail.com'

SELECT * FROM users WHERE edad >25 OR email='adriana@gmail.com'

SELECT * FROM users WHERE edad!=25

SELECT * FROM users WHERE edad BETWEEN 15 AND 27;

SELECT * FROM users WHERE email LIKE '%gmail%';

SELECT * FROM users WHERE email LIKE 'oscar%';

SELECT * FROM users WHERE email LIKE '%gmail';

SELECT * FROM users ORDER BY edad ASC;

SELECT * FROM users ORDER BY edad DESC;

SELECT * FROM users WHERE edad = (SELECT MIN(edad) FROM users);

SELECT *FROM users WHERE edad = (SELECT MAX(edad) FROM users);

SELECT id, name from users;

select id, name as nombre from users;

select id as identificacion, name as nombre, email  from users;

-- joins 

create table products(
id int identity (1,1),
name varchar(255) not null,
create_by int not null,
marca varchar(255) not null,
primary key (id), 
foreign key (create_by) references users (id)
);

select * from products

-- para renombrar
EXEC sp_rename 'products', 'product';

select * from product;

-- insertar datos para jugar
INSERT INTO product(name,create_by,marca)
values
('ipad',1,'apple'),
('iphone',1,'apple'),
('watch',2,'apple'),
('macbook',1,'apple'),
('imac',3,'apple'),
('ipad mini',2,'apple');

select * from product;

-- left join
select u.id,u.email, p.name from users u left join product p on u.id = p.create_by;

-- right join
select u.id,u.email,p.name from users u right join product p on u.id = p.create_by;

-- inner join 
select u.id,u.email,p.name from users u inner join product p on u.id = p.create_by;

-- cross join 
select u.id,u.name,p.id,p.name from users u cross join product p;

-- group by 
select count (id),marca from product group by marca;

SELECT COUNT(p.id) AS product_count, u.name
FROM product p
LEFT JOIN users u ON u.id = p.create_by
GROUP BY u.name;

SELECT COUNT(p.id) AS product_count, u.name
FROM product p
LEFT JOIN users u ON u.id = p.create_by
GROUP BY u.name
having count(p.id)>=2;


drop table product

