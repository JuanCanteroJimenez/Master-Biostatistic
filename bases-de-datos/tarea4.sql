CREATE TABLE animales (
    tipoanimal INTEGER,
    nombre VARCHAR(15),
    PRIMARY KEY (tipoanimal)
)

CREATE TABLE proveedores (
    idproveedor INTEGER,
    nombre VARCHAR(30),
    notas VARCHAR(50),
    email VARCHAR(20),
    telefono VARCHAR(9),
    PRIMARY KEY (idproveedor)

)

CREATE TABLE clientes (
    idcliente INTEGER,
    nombre VARCHAR(30),
    apellido VARCHAR(30),
    email VARCHAR(30),
    direccionpostal VARCHAR(40),
    ciudad VARCHAR(30),
    telefono VARCHAR(9),
    PRIMARY KEY (idcliente)
)

CREATE TABLE empleados (
    idempleado INTEGER,
    nombre VARCHAR(20),
    apellidos VARCHAR(30),
    email VARCHAR(30),
    direccion VARCHAR(30),
    telefono VARCHAR(9),
    trabajo VARCHAR(20),
    salario DECIMAL(7,2),
    PRIMARY KEY (idempleado)
)

CREATE TABLE mascotas (
    idmascota INTEGER,
    idcliente INTEGER,
    nombremascota VARCHAR(20),
    tipoanimal INTEGER,
    fechanac DATE,
    PRIMARY KEY (idmascota),
    FOREIGN KEY (idcliente) REFERENCES clientes(idcliente) ON DELETE CASCADE,
    FOREIGN KEY (tipoanimal) REFERENCES animales(tipoanimal) ON DELETE CASCADE
)

CREATE TABLE productos (
    idproducto INTEGER,
    descripcion VARCHAR(40),
    precio_compra DECIMAL(7,2),
    precio_venta DECIMAL(7,2),
    stock INTEGER,
    idproveedor INTEGER,
    PRIMARY KEY (idproducto),
    FOREIGN KEY (idproveedor) REFERENCES proveedores(idproveedor) ON DELETE CASCADE
)

CREATE TABLE pedidos (
    idpedido INTEGER,
    empleadoventa INTEGER,
    idcliente INTEGER,
    fechapedido DATE,
    fechaentrega DATE,
    direccionentrega VARCHAR(40),
    PRIMARY KEY (idpedido),
    FOREIGN KEY (empleadoventa) REFERENCES empleados(idempleado) ON DELETE CASCADE,
    FOREIGN KEY (idcliente) REFERENCES clientes(idcliente) ON DELETE CASCADE
)
--si no funciona quitar una de las primarias
CREATE TABLE pedidos_productos (
    idpedido INTEGER,
    idproducto INTEGER,
    cantidad INTEGER,
    precioreal DECIMAL(7,2),
    PRIMARY KEY (idpedido,idproducto),
    FOREIGN KEY (idpedido) REFERENCES pedidos(idpedido) ON DELETE CASCADE,
    FOREIGN KEY (idproducto) REFERENCES productos(idproducto) ON DELETE CASCADE
)

CREATE TABLE cursos (
    idcurso INTEGER,
    titulo VARCHAR(40),
    descripcion VARCHAR(100),
    max_num INTEGER,
    precio DECIMAL(7,2),
    tipoanimal INTEGER,
    fechahora DATE,
    lugar VARCHAR(30),
    instructor INTEGER,
    PRIMARY KEY (idcurso),
    FOREIGN KEY (tipoanimal) REFERENCES animales(tipoanimal) ON DELETE CASCADE,
    FOREIGN KEY (instructor) REFERENCES empleados(idempleado) ON DELETE CASCADE,
)
--si no funciona quitar una de las primarias
CREATE TABLE cursos_clientes (
    idcurso INTEGER,
    idcliente INTEGER,
    PRIMARY KEY (idcurso,idcliente),
    FOREIGN KEY (idcurso) REFERENCES cursos(idcurso) ON DELETE CASCADE,
    FOREIGN KEY (idcliente) REFERENCES clientes(idcliente) ON DELETE CASCADE,
)