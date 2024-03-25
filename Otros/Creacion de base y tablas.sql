DROP DATABASE Basepractica; --Para casos de errores podemos eliminar la base 

-- Crear la base de datos
CREATE DATABASE Basepractica;

-- Usar la base de datos recién creada
USE Basepractica;
GO

-- Crear la tabla de factura
CREATE TABLE Factura (
    NumeroFactura INT IDENTITY(1,1), -- esta es mi llave primaria, el numero de factura debe ser positivo ademas de que no puede faltar, ademas genera un id
    Fecha DATE NOT NULL, -- la fecha es importante para grantias y demas por ello no pude faltar
    UnidadesCompradas INT NOT NULL,  
	Precio DECIMAL(10,2) NOT NULL,--precio puede tener decimales al igual que los impuestos
	PorcentImpuestos DECIMAL(10,2) NOT NULL,
	PorcentDescuento DECIMAL(10,2) NOT NULL -- para factura es muy importante el not null para controlar garantias y demas
	PRIMARY KEY (NumeroFactura)--declaramos la llave primaria
);

-- Verificamos su creacion 
--SELECT * 
--FROM Factura

-- Crear la tabla de cliente
CREATE TABLE Cliente (
    Cedula INT NOT NULL CHECK (Cedula >= 0), -- cedula es llave primaria, no puede estar vacio ni ser negativo, es entero
    Compra INT NOT NULL,--futura llave foranea
	Tipocedula VARCHAR(255) NOT NULL, -- tipocedula es caracter (fisica,juridica)
    Email VARCHAR(255), -- email nombre y direccion es de tipo caracter
	Nombre VARCHAR(255)NOT NULL,
	Direccion VARCHAR(255),
	PRIMARY KEY (Cedula),-- declaramos la llave primaria
	FOREIGN key (Compra) REFERENCES Factura (NumeroFactura) -- declaramos la llave foranea
);

-- Verificamos su creacion
--SELECT *
--FROM Cliente

-- Crear la tabla de Categoria
CREATE TABLE Categoria (
    Nombre VARCHAR(255)  NOT NULL,-- esta es nuestra llave primaria
    Tamano VARCHAR(255),
	Precio DECIMAL(10,2) NOT NULL, -- el precio no puede faltar
	Color VARCHAR(255),
	PRIMARY KEY(Nombre) -- declaramos la llave primaria
);

-- Verificamos su creacion 
--SELECT * 
--FROM Categoria

-- Crear la tabla de subcategoria
CREATE TABLE Subcategoria (
    IDcategoria INT  IDENTITY(1,1) NOT NULL, -- esta llave primaria no puede estar vacia ni ser negativa, genera id tipo int
    Tamano VARCHAR(255),
	Categorizacate VARCHAR(255) NOT NULL,-- futura foranea
	Precio DECIMAL(10,2) NOT NULL,
	Nombre VARCHAR(255) NOT NULL, -- preferiblemente ni precio ni nombre debe de estar vacio
	Color VARCHAR(255),
	PRIMARY KEY (IDcategoria),-- declaramos la llave primaria
	FOREIGN key (Categorizacate) REFERENCES Categoria (Nombre) -- declaramos la llave foranea
);
-- Verificamos su creacion 
--SELECT * 
--FROM Subcategoria

-- Crear la tabla de productos
CREATE TABLE Producto (
    IDunico INT IDENTITY (1,1)NOT NULL, -- esta es mi llave primaria, debe ser positiva, genera un id
    IDuniversal INT,--- no incluimos el not null en este caso, el producto puede ser generico y no tenerlo
    Categorizasub INT NOT NULL,-- futura foranea
	Tamano VARCHAR(255),
	Precio DECIMAL(10,2) NOT NULL, -- el precio no puede faltar 
	Nombre VARCHAR(255) NOT NULL, -- de igual manera el nombre no puede faltar
	Color VARCHAR(255),
	PRIMARY KEY(IDunico), --declaramos la lleve primaria
	FOREIGN key (Categorizasub) REFERENCES Subcategoria (IDcategoria) -- declaramos la llave foranea

);

-- Verificamos su creacion 
--SELECT * 
--FROM Producto

-- Crear la tabla de detalles de Proveedor
CREATE TABLE Proveedor (
    Cedula INT PRIMARY KEY NOT NULL CHECK(Cedula>=0),-- esta es mi llave primaria con sus respectivas restricciones
    IDterritorio INT NOT NULL,-- esta variable es de interes por ello el not null al igual que en telefono nombre etc 
    Nombre VARCHAR(255)NOT NULL,
    Telefono INT NOT NULL,
	Email VARCHAR(255),
	UnidadesCompradas INT NOT NULL, -- necesitamos que las unidades compradas no este vacio
	Tipocedula VARCHAR(255) NOT NULL -- para este caso la mayoria de informacion es importante por ello el not null 
);
-- Verificamos su creacion
--SELECT * 
--FROM Proveedor

-- Crear la tabla de detalles de territorio
CREATE TABLE Territorio (
    IDubicacion INT IDENTITY(1,1) NOT NULL, -- esta es mi llave primaria debe de ser positivo y no puede estar vacio
    Ubica INT NOT NULL, -- futura foranea
	Provincia VARCHAR(255) NOT NULL, -- variables de interes para ubicacion
    Distrito VARCHAR(255)NOT NULL, 
    Canton VARCHAR(255)NOT NULL,
	PRIMARY KEY (IDubicacion), -- declaramos la llave primaria
	FOREIGN KEY (Ubica) REFERENCES Proveedor (Cedula) -- declaramos la lleve foranea

);

-- Verificamos su creacion 
--SELECT * 
--FROM Territorio


-- RELACIONES ENTRE ENTIDADES

-- tabla relacion factura-producto
-- declaramos la primaria ademas de las foranea necesarias para la tabla intermedia para relaciones n:m
CREATE TABLE Transaccion (
    IDTransaccion INT,
    IDFactura INT,
    IDProducto INT,
    Cantidad INT,
	PRIMARY KEY (IDTransaccion), --llave primaria
    FOREIGN KEY (IDFactura) REFERENCES Factura (NumeroFactura), -- llaves foraneas
    FOREIGN KEY (IDProducto) REFERENCES Producto (IDunico)
);

-- tabla de relacion proveedor-producto
-- declaramos la primaria ademas de las foranea necesarias para la tabla intermedia para relaciones n:m
 CREATE TABLE Vende (
    IDvende INT NOT NULL,
    IDProveedor INT,
    IDProducto int,
	PRIMARY KEY (IDvende), -- funciona igual que la anterior
    FOREIGN KEY (IDProveedor) REFERENCES Proveedor (Cedula),
    FOREIGN KEY (IDProducto) REFERENCES Producto (IDunico)
);

-- INSERTANDO LOS DATOS
-- Insertar datos en la tabla Factura
-- declaramos a que variable corresponde cada espacio y abajo los inseratamos segun el espacio que inticamos en el insert donde id
-- es generado, no es necesario crear una variable para el mismo ya que es autogenerado
INSERT INTO Factura ( Fecha, UnidadesCompradas, Precio, PorcentImpuestos, PorcentDescuento) 
VALUES
    ( '2023-09-01', 5, 49.99, 10, 5),
    ('2023-08-02', 3, 29.99, 8, 0),
    ( '2023-07-03', 2, 39.99, 12, 3),
    ('2023-08-04', 7, 19.99, 5, 2),
    ('2023-08-05', 4, 59.99, 15, 0),
    ('2023-07-06', 1, 9.99, 0, 0),
    ('2023-09-07', 6, 89.99, 20, 10),
    ('2023-09-08', 2, 34.99, 8, 0),
    ('2023-02-09', 8, 44.99, 10, 5),
    ('2023-09-10', 3, 14.99, 5, 1);
-- Probamos a ver como quedo 
--select * 
--from Factura

-- Llenar datos de prueba para cliente
-- esta y las demas funciona igual su sintaxis a la primera insercion de datos
INSERT INTO Cliente (Cedula,Tipocedula, Email,Nombre,Direccion,Compra) 
VALUES
    (12345, 'Juridica', 'cliente1@email.com', 'Juan Pérez', 'Calle A #123',1),
    (23456, 'Fisica', 'cliente2@email.com', 'María García', 'Avenida B #456',2),
    (34567, 'Juridica', 'cliente3@email.com', 'Carlos Rodríguez', 'Calle C #789',3),
    (45678, 'Fisica', 'cliente4@email.com', 'Ana Martínez', 'Avenida D #1011',4),
    (56789, 'Juridica', 'cliente5@email.com', 'Luis González', 'Calle E #1213',5),
    (67890, 'Fisica', 'cliente6@email.com', 'Laura Hernández', 'Avenida F #1415',6),
    (78901, 'Juridica', 'cliente7@email.com', 'Jorge López', 'Calle G #1617',7),
    (89012, 'Fisica', 'cliente8@email.com', 'Patricia Ramírez', 'Avenida H #1819',8),
    (90123, 'Fisica', 'cliente9@email.com', 'Manuel Sánchez', 'Calle I #2021',9),
    (12365, 'Juridica', 'cliente10@email.com', 'Carmen Flores', 'Avenida J #2223',10);
-- Vemos los datos insertados 
--SELECT* 
--FROM Cliente

-- Llenar datos de Categoria
INSERT INTO Categoria(Nombre, Tamano,Precio,Color)
VALUES
    ('Zapatos', 'Grande', 199.99, 'Negro'),
    ('Camisetas', 'Mediano', 29.99, 'Azul'),
    ('Mochilas', 'Grande', 49.99, 'Verde'),
    ('Gorras', 'Pequeño', 14.99, 'Rojo'),
    ('Pantalones', 'Mediano', 19.99, 'Amarillo'),
    ('Sudaderas', 'Grande', 59.99, 'Morado'),
    ('Calcetines', 'Pequeño', 9.99, 'Rosa'),
    ('Chaquetas', 'Mediano', 89.99, 'Negro'),
    ('Bolsas', 'Grande', 39.99, 'Blanco'),
    ('Bufandas', 'Pequeño', 12.99, 'Gris');

-- Probamos a ver como quedo 
--select * 
--from Categoria

-- Llenar datos de Subcategoria
INSERT INTO Subcategoria(Categorizacate,Tamano,Precio,Nombre,Color)
VALUES
    ('Zapatos','Grande', 199.99,'Producto 1', 'Negro'),
    ('Camisetas','Mediano', 29.99, 'Producto 2','Azul'),
    ('Mochilas','Grande', 49.99, 'Producto 3','Verde'),
    ('Gorras', 'Pequeño', 14.99, 'Producto 4','Rojo'),
    ('Pantalones', 'Mediano', 19.99, 'Producto 5','Amarillo'),
    ('Sudaderas','Grande', 59.99,'Producto 6', 'Morado'),
    ('Calcetines', 'Pequeño', 9.99,'Producto 7', 'Rosa'),
    ('Chaquetas', 'Mediano', 89.99, 'Producto 8','Negro'),
    ('Bolsas','Grande', 39.99, 'Producto 9','Blanco'),
    ('Bufandas','Pequeño', 12.99, 'Producto 10','Gris');
-- Probamos a ver como quedo 
--select * 
--from Subcategoria

-- Llenar datos de prueba para Producto 
INSERT INTO Producto(IDuniversal, Tamano,Precio,Nombre,Color,Categorizasub)
VALUES
    ( 12001, 'Pequeño', 9.99, 'Producto A', 'Rojo',1),
    ( 11002, 'Mediano', 14.99, 'Producto B', 'Azul',2),
    ( 12003, 'Grande', 19.99, 'Producto C', 'Verde',3),
    ( 13004, 'Pequeño', 8.99, 'Producto D', 'Amarillo',4),
    ( 14005, 'Mediano', 12.99, 'Producto E', 'Naranja',5),
    ( 15006, 'Grande', 17.99, 'Producto F', 'Morado',6),
    ( 16007, 'Pequeño', 10.99, 'Producto G', 'Rosa',7),
    ( 17008, 'Mediano', 13.99, 'Producto H', 'Negro',8),
    ( 18009, 'Grande', 16.99, 'Producto I', 'Blanco',9),
    ( 19010, 'Pequeño', 11.99, 'Producto J', 'Gris',10);

-- Vemos los datos insertados 
--SELECT* 
--FROM Producto

-- Insertar datos en la tabla Proveedor
INSERT INTO Proveedor (Cedula, IDterritorio, Nombre, Telefono, Email, Tipocedula,UnidadesCompradas)
VALUES
    (12345, 1, 'Proveedor A', 22223333, 'proveedora@email.com', 'Juridica',10),
    (98765, 2, 'Proveedor B', 44445555, 'proveedorb@email.com', 'Fisica',20),
    (55555, 3, 'Proveedor C', 66667777, 'proveedorc@email.com', 'Juridica',30),
    (11111, 4, 'Proveedor D', 99998888, 'proveedord@email.com', 'Juridica',80),
    (99999, 5, 'Proveedor E', 11112222, 'proveedore@email.com', 'Fisica',40),
    (88888, 6, 'Proveedor F', 77776666, 'proveedorf@email.com', 'Fisica',90),
    (77777, 7, 'Proveedor G', 55554444, 'proveedorg@email.com', 'Fisica',10),
    (66666, 8, 'Proveedor H', 33332222, 'proveedorh@email.com', 'Juridica',30),
    (44444, 9, 'Proveedor I', 11119999, 'proveedori@email.com', 'Juridica',90),
    (22222, 10, 'Proveedor J', 88887777, 'proveedorj@email.com', 'Fisica',130),
    (44433, 11, 'Proveedor K', 55554344, 'proveedorK@email.com', 'Fisica',15),
    (44435, 12, 'Proveedor L', 33332322, 'proveedorL@email.com', 'Juridica',20),
    (44436, 13, 'Proveedor M', 11119399, 'proveedorM@email.com', 'Juridica',60),
    (22237, 14, 'Proveedor O', 88887377, 'proveedorO@email.com', 'Fisica',110);
-- Probamos a ver como quedo 
--select * 
--from Proveedor

-- Insertar datos en la tabla Territorio
INSERT INTO Territorio (Ubica,Provincia, Distrito, Canton)
VALUES
    (12345,'San José', 'Alajuelita', 'Alajuelita'),
    (98765,' San José', 'San Ignacio', 'Acosta'),
    (55555,' San José', 'San Juan', ' Tibás'),
    (11111,'San José', ' San Vicente', ' Moravia'),
    (99999,'San José', 'San Pablo', ' Turrubares'),
    (88888,'San José', ' Jardín', ' Dota'),
    (77777,'San José', 'Curridabat', 'Curridabat'),
    (66666,'Alajuela', 'Alajuela', 'Alajuela'),
    (44444,'Alajuela', 'Grecia', 'Grecia'),
	(22222,'Alajuela', 'Naranjo', 'Naranjo'),
	(44433,'Alajuela', 'Palmares', 'Palmares'),
	(44435,'Alajuela', 'Coyolar', 'Orotina'),
	(44436,'Alajuela', 'Zarcero', 'Zarcero'),
    (22237,'Alajuela', 'Upala', 'Upala');

-- Probamos a ver como quedo 
--select * 
--from Territorio


-- CONSULTAS DE INTERES

--Cuánto dinero se ha vendido en total por mes?
SELECT
    DATEPART(MONTH, Fecha) AS Mes,
    DATEPART(YEAR, Fecha) AS Año,
    SUM(Precio) AS TotalVentas
FROM
    Factura
GROUP BY
    DATEPART(YEAR, Fecha),
    DATEPART(MONTH, Fecha)
ORDER BY
    Año, Mes;

--Cuántos productos diferentes tiene la empresa?
SELECT COUNT(DISTINCT IDunico) AS TotalProductos
FROM Producto;

--Cuáles son los proveedores a los que más unidades de productos se les compra?
SELECT *
FROM Basepractica.dbo.Proveedor
ORDER BY UnidadesCompradas DESC;

-- OTRA MANERA
SELECT * FROM Proveedor WHERE UnidadesCompradas = (SELECT MAX(UnidadesCompradas) FROM Proveedor);

--Cuáles son los clientes a los que más unidades de productos se les vende?
SELECT c.Compra, c.Nombre, MAX(v.UnidadesCompradas) AS total_unidades_vendidas
FROM Cliente c
JOIN Factura v ON c.Compra = v.NumeroFactura
GROUP BY c.Compra, c.Nombre
ORDER BY total_unidades_vendidas DESC;

-- Qué categoría de producto vende menos?
-- intento 1
SELECT c.IDcategoria, c.Nombre, MIN(v.UnidadesCompradas) AS total_unidades_vendidas
FROM Subcategoria c
JOIN Factura v ON c.IDcategoria = v.NumeroFactura
GROUP BY c.IDcategoria, c.Nombre
ORDER BY total_unidades_vendidas;


-- consulta correcta
SELECT c.IDcategoria, c.Categorizacate, MIN(v.UnidadesCompradas) AS total_unidades_vendidas
FROM Subcategoria c
JOIN Factura v ON c.IDcategoria = v.NumeroFactura
GROUP BY c.IDcategoria, c.Categorizacate
ORDER BY total_unidades_vendidas;

-- confirmamos
SELECT * FROM Subcategoria WHERE IDcategoria = 6 ;
--Entre otras que se puedan realizar en tiempo de revisión.
