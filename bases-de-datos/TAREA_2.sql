--1
select * from v_mascotas;
--2
select idproducto, descripcion, (precio_venta-precio_compra) from v_productos order by 3 desc;
--3
select distinct trabajo from v_empleados order by trabajo asc;
--4
select nombre,apellido from v_clientes where ciudad = 'Burjassot' order by 2 asc;
--5
select nombre,apellidos from v_empleados where salario between 1500 and 2000;
--6
select nombre,apellido,email from v_clientes where apellido = 'GARCÍA';
--7
select nombre,apellido,email from v_clientes where email like '%@uv.es' or email like '%@alumni.uv.es';
--8
select * from v_clientes where email is Null;
--9
select * from v_productos where precio_venta>20 and stock<10 order by stock;
--10
select nombre,apellido,titulo
from v_cursos
join v_cursos_clientes on (v_cursos.idcurso = v_cursos_clientes.idcurso)
join v_clientes on (v_clientes.idcliente = v_cursos_clientes.idcliente)
order by 2 asc, 3 asc  ;
--11
select nombremascota,fechanac
from v_mascotas
join v_animales on (v_mascotas.tipoanimal = v_animales.tipoanimal)
where v_animales.nombre in ('Perro','Gato')
order by v_animales.tipoanimal asc, v_mascotas.fechanac asc;
--12
select nombre,apellidos
from v_empleados
union
select nombre, apellido
from v_clientes
order by apellidos;
--13
select nombre,apellido
from v_clientes
minus
select nombre,apellido
from v_clientes
join v_cursos_clientes on (v_clientes.idcliente = v_cursos_clientes.idcliente)
join v_cursos on (v_cursos.idcurso = v_cursos_clientes.idcurso)
where titulo = 'Como tratar una mascota';
--14
select v_clientes.nombre, v_clientes.apellido
from v_clientes, v_clientes v_clientes2
where v_clientes.nombre = v_clientes2.nombre
and v_clientes.apellido not in v_clientes2.apellido;
--15
