INSERT INTO animales(tipoanimal,nombre) VALUES
(1,'Gato'),
(2,'Perro'),
(3,'Cerdo'),
(4,'Oveja'),
(5,'Caballo'),
(6,'Hamster'),
(7,'Loro'),
(8,'Conejo'),
(9,'Canario'),
(10,'Periquito'),
(11,'Paloma');

INSERT INTO clientes(idcliente,nombre,apellido,email,direccionpostal,ciudad,telefono) VALUES
(1,'JUAN','MARTÍN','jm@gmail.com','Colon, 12','Valencia','963547873'),
(2,'JUAN','LÓPEZ','ml@hotmail.com','Ruzafa, 18','Valencia','963454545'),
(3,'ELENA','SALGUERO','es@hotmail.com','Ruzafa, 13','Burjassot','967343434'),
(4,'MARÍA','DÍAZ',null,'San Vicente, 20','Valencia','964686869'),
(5,'MARÍA','GARCÍA','mg@gmail.com','Malaga, 53','Valencia','963565656'),
(6,'FRANCISCO','GIMÉNEZ','fg@gmail.com','Reus, 38','Valencia','639777777'),
(7,'FEDERICO','GIL',null,'Liria, 37','Burjassot','687778789'),
(8,'CARMEN','ALBERO','ca@hotmail.com','Mayor, 2','Burjassot','656789090'),
(9,'SARA ','ALBERTO','sa@gmail.com','Menor, 3','Liria','645342434'),
(10,'PEDRO','RUEDA','pr@gmail.com','Avd. Valencia','Liria','654121234'),
(11,'ESTHER ','DE VES','esther.deves@uv.es','Almazora, 23','Valencia','654123123'),
(12,'ALGO','VACIO',null,null,null,'632123123'),
(14,'ANA MARÍA','FERNÁNDEZ','anafe@alumni.uv.es','Calle del mediterraneo','Betera','676777888'),
(13,'ALBA','ALBA','alba@gmail.com','nada','Valencia','632254545'),
(16,'NICOLAS JUAN','CAMPOS',null,null,'Betera','657877879');

INSERT INTO cursos(idcurso,titulo,descripcion,max_num,precio,tipoanimal,fechahora,lugar,instructor) VALUES
(9,'Gatos en casa','Curso inicial de los cuidados de los gatos',20,50.00,1,'17/07/17','sala 3',null),
(11,'Los secretos de los animales domesticos','Curso de divulgación general',10,35.00,null,'03/09/17','sala 2',2),
(1,'Educando cachorros','Curso para cualquier mascota',20,80.00,null,'20/07/16','sala 1',7),
(2,'Adiestrando grandes perros','Curso para evitar perros violentos',12,100.00,2,'21/08/17','sala 2',1),
(3,'Higiene pequeños perros','Curso para perros pequeños que salen poco de casa',10,75.00,2,'21/08/16','sala 1',2),
(4,'Higiene de gatos especiales','Curso para ayudar a cuidar a gatos con necesidades especiales',15,80.00,1,'05/08/17','sala 1',7),
(5,'Necesidades gatos comunes','Curso para aprender a cuidar a un gato común',10,60.00,1,'06/08/17','sala 3',1),
(6,'Domesticar a gatos como perros','Cómo hacer que los gatos obedezcan.',15,60.00,1,'07/08/17','sala 1',1),
(7,'La alimentacion de los conejos','Curso para aprender una buena alimentacion de los conejos domesticos',20,75.00,8,'28/07/17','sala 3',7),
(8,'Adiestrande grandes perros 2','Curso avanzado sobre como manejar los perros violentos',12,100.00,2,'16/09/17','sala 1',7),
(10,'Como tratar una mascota','Curso para principiantes',10,50.00,2,'30/07/18','sala 1',7),
(12,'Dieta sana para GATOS','Curso básico',20,50.00,1,'03/08/18','sala 2',1),
(13,'Dieta sana para perros','Curso básico',10,50.00,2,'03/07/18','sala 1',1),
(14,'Dieta sana para perros (segunda edicion)','Curso imprescindible',25,60.00,2,'02/07/18','sala 4',1),
(15,'Como tratar una mascota genérica','Curso básico',30,55.00,null,'10/07/18','sala 3',null),
(16,'La alimentacion de los periquitos','Curso sobe los mejores alpistes',15,35.00,10,'20/07/18','sala 1',7);

INSERT INTO cursos_clientes(idcurso,idcliente) VALUES
(1,1),
(1,2),
(1,3),
(1,7),
(2,1),
(2,3),
(2,4),
(2,5),
(2,13),
(3,1),
(3,3),
(4,3),
(4,7),
(4,8),
(5,1),
(5,2),
(5,5),
(5,7),
(5,8),
(6,6),
(6,7),
(6,8),
(7,3),
(8,1),
(8,2),
(8,3),
(8,4),
(8,5),
(8,6),
(8,8),
(9,9),
(9,12),
(10,1),
(10,2),
(10,3),
(10,4),
(10,5),
(10,7),
(10,8),
(10,10),
(10,11),
(10,12),
(11,1),
(11,2),
(11,3),
(13,1),
(13,2),
(13,3),
(13,4),
(13,5),
(13,6);

INSERT INTO empleados(idempleado,nombre,apellidos,email,direccion,telefono,trabajo,salario) VALUES
(1,'Juan','Díaz','jdiaz@domes.es','Colon, 3, Valencia','963676767','instructor',1300.00),
(2,'Miguel','Pardo','mpardo@domes.es','Jativa, 13, Valencia','963545454','instructor',1400.00),
(3,'Miguel','Pérez','mperez@domes.es','Guillem, 22, Valencia','963543323','veterinario',2300.00),
(4,'Alicia','San Juan','asanjuan@domes.es','Mayor, 3, Burjassot','962656555','veterinario',2400.00),
(5,'Fernando','Saez','fsaez@domes.es','Moliner, 5, Burjassot','962676667','comercial',950.00),
(6,'Jose','García','jgarcia@domes.es','Poeta Artola, 7, Valencia','963456546','comercial',1200.00),
(7,'Juan','Martínez','jmartinez@domes.es','Moliner,7, Burjassot','962878787','instructor',1500.00),
(8,'Clara','Fernández','clfernandez@gmail.com','Corredera, 8, Liria','639776565','comercial',1400.00),
(9,'Isabel ','Cuenca','icuenca@hotmail.com','Corredera, 38, Valencia','967656566','administrativo',1350.00),
(10,'Juana','López','juanlo@gmail.com','Moliner, 3, Burjassot','646897678','comercial',1450.00);

INSERT INTO mascotas(idmascota,idcliente,nombremascota,tipoanimal,fechanac) VALUES
(1,1,'Pepito',1,'26/03/08'),
(3,1,'Flipo',1,'22/05/10'),
(2,2,'Mara',2,'22/02/07'),
(4,2,'Flipa',4,'08/02/11'),
(5,3,'Misia',3,'07/03/11'),
(6,4,'Saturno',5,'01/01/11'),
(7,5,'Mansito',2,'03/05/08'),
(8,7,'Fortachon',10,'05/05/08'),
(9,8,'Fresita',1,'05/06/09');

INSERT INTO pedidos(idpedido,idcliente,fechapedido,fechaentrega,direccionentrega,empleadoventa) VALUES
(1,1,'25/03/16','19/04/16','C/ Colon, 3, Valencia',5),
(2,2,'28/04/16','23/05/16','C/ Jativa, 10, Valencia',6),
(3,1,'23/03/17','17/04/17','C/ Reus, 3, Valencia',10),
(4,2,'05/05/17','30/05/17','Avd. Burjassot, 10, Valencia',10),
(5,3,'11/05/17','05/06/17','Avd. Constitucion, 14, Valencia',10),
(6,4,'21/05/17','15/06/17','Avd. Constitucion, 20, Valencia',5),
(7,5,'31/05/17','25/06/17','C/ Malaga, 3, Valencia',5),
(8,5,'09/06/16','04/07/16','C/ Poeta Artola, 2, Valencia',6),
(9,1,'20/04/17','15/05/17','C/ Poeta Querol, 3, Valencia',10),
(10,1,'31/01/17','25/02/17','C/ Miguel Delibes, 4, Burjassot',10),
(11,5,'25/02/17','22/03/17','C/ Malaga, 3, Valencia',10),
(12,5,'27/02/17','24/03/17','C/Malaga, 3, Valencia',10),
(13,4,'10/09/17',null,'Avda. Universitat s/n',6),
(14,2,'07/01/17','01/02/17','Avd. Burjassot, 10, Valencia',5),
(15,1,'05/10/17',null,'C/ Colon, 3, Valencia',5),
(16,5,'04/04/17','29/04/17','C/ Malaga, 3, Valencia',6);

INSERT INTO pedidos_productos(idpedido,idproducto,cantidad,precioreal) VALUES
(1,1,3,13.41),
(2,3,1,28.17),
(2,4,3,17.01),
(2,5,1,13.50),
(3,6,4,11.07),
(4,7,5,8.37),
(5,1,3,13.41),
(5,2,3,8.01),
(5,6,2,11.07),
(6,7,8,8.37),
(7,3,2,28.17),
(8,1,4,13.41),
(9,2,3,8.01),
(10,1,2,13.41),
(10,2,3,8.01),
(11,1,4,13.41),
(12,5,1,13.50),
(5,5,4,13.50),
(13,1,5,13.50),
(13,2,6,8.03),
(13,4,10,11.08),
(14,2,11,8.00),
(15,7,3,8.01),
(15,6,10,11.07),
(15,1,3,13.30),
(16,3,5,28.00),
(16,4,3,17.02),
(16,2,10,7.99);

INSERT INTO productos(idproducto,descripcion,precio_compra,precio_venta,stock,idproveedor) VALUES
(1,'Comida Gatos, 5kg',10.80,14.90,50,1),
(2,'Comida Periquitos, 1Kg',6.30,8.90,40,1),
(20,'Comida Perros grandes, 10 kg',23.40,32.10,20,1),
(3,'Comida Perros medianos completa, 5 kg',22.30,31.30,20,1),
(4,'Comida Loros, 2 Kg',10.90,18.90,30,2),
(5,'Comida Conejos, salud, 5 Kg',10.80,15.00,10,2),
(6,'Amoxicilina, 10 mg',8.00,12.30,6,3),
(7,'Parocetamol 200 mg, soluble',7.20,9.30,10,3),
(8,'Champu canino 1000 ml',12.30,17.80,6,3),
(9,'Champu especial gatos, 500 ml',8.30,13.50,8,3),
(10,'Limpieza en seco gatos, 300 ml',7.90,10.30,22,3),
(11,'Trasportador perros, grande',33.90,60.00,8,1),
(12,'Trasportador perros, mediano',23.80,43.20,6,1),
(13,'Trasportador gatos, cachorros',23.80,32.30,4,1),
(14,'Trasportador gatos, adultos',34.80,49.30,8,2),
(15,'Cesta pequeña, gatos y perros',23.30,34.80,5,2),
(16,'Cesta grande, gatos y perros',28.30,37.80,7,2);

INSERT INTO proveedores(idproveedor,nombre,notas,email,telefono) VALUES
(1,'ProdAnimal, S. L.',null,'pa@gmail.com','967656565'),
(2,'ProVeter, S.L',null,'pv@hotmail.com','963456554'),
(3,'Cuidados Animales, S.L',null,'ca@hotmail.com','965434343'),
(5,'VentaParaAnimales, S. A',null,null,'639706865'),
(6,'Mascotines, S.L.','Muy rápidos en su servicio','mascotines@gmail.com',null),
(4,'ProductZoo, S.L.',null,'elgato@gmail.com','963787878');


