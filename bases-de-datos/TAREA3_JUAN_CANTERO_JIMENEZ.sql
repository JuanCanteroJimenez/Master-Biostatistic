--1
select avg(precio), min(precio), max(precio) from v_cursos;
--2
select count(distinct ciudad) from v_clientes;
--3
select count(idcurso) from v_cursos where lugar = 'sala 3';
--4
select v_animales.nombre, count(distinct v_mascotas.nombremascota)
from v_mascotas join v_animales on (v_mascotas.tipoanimal = v_animales.tipoanimal)
group by v_animales.nombre order by v_animales.nombre;
--5
select nombre, count(idcurso), max(precio), min(precio), avg(precio)
from v_cursos left join v_animales on (v_cursos.tipoanimal = v_animales.tipoanimal)
group by nombre;
--6
select distinct nombre, apellidos 
from v_empleados join v_cursos on (idempleado = instructor) 
where idempleado in (select idempleado 
from v_empleados join v_cursos on (idempleado = instructor)
group by idempleado having count(idempleado) > 3);

--7
select titulo, max_num from v_cursos 
join (select count(idcurso) plazas_ocup, idcurso from v_cursos_clientes group by idcurso) inter on (v_cursos.idcurso = inter.idcurso ) 
where v_cursos.max_num = plazas_ocup;
--8
select nombremascota, fechanac from v_mascotas where fechanac = (select min(fechanac) from v_mascotas);
--9
select nombre, apellidos from v_empleados where salario > (select avg(salario) from v_empleados);
--10
select nombre 
from v_productos right join v_proveedores on (v_productos.idproveedor = v_proveedores.idproveedor)
where idproducto is null;
--11
select v_empleados.nombre, v_empleados.apellidos, v_empleados.trabajo, v_empleados.salario
from v_empleados join
(select trabajo, max(salario) msalario from v_empleados group by trabajo) maximos on 
(v_empleados.trabajo = maximos.trabajo and v_empleados.salario = maximos.msalario) order by v_empleados.salario;
--12
select nombre, apellidos, salario 
from v_empleados 
where salario > (select max(salario) from v_empleados where nombre = 'Juan')and
salario < (select min(salario) from v_empleados where nombre = 'Alicia');
--13
select nombre
from v_clientes 
where idcliente in 
(select todos
from 
((select  v_cursos_clientes.idcliente cpedidos, v_clientes.idcliente todos 
from v_cursos_clientes full join v_clientes on (v_cursos_clientes.idcliente = v_clientes.idcliente))
intersect
(select v_pedidos.idcliente cpedidos, v_clientes.idcliente todos 
from v_pedidos right join v_clientes on (v_pedidos.idcliente = v_clientes.idcliente)))  where cpedidos is null);
--14
select distinct v_clientes.nombre, v_clientes.apellido
from 
v_pedidos_productos join v_pedidos on (v_pedidos_productos.idpedido = v_pedidos.idpedido)
join v_clientes on (v_pedidos.idcliente = v_clientes.idcliente)
where v_pedidos_productos.idpedido = (select idpedido
from (select idpedido, sum(cantidad*precioreal) precio 
from v_pedidos_productos group by idpedido) where precio = (select max(precio) 
from (select idpedido, sum(cantidad*precioreal) precio from v_pedidos_productos group by idpedido)));

--15
select count(email) from ((select email from v_empleados) union (select email from v_clientes)) where email like '%@gmail.com';
