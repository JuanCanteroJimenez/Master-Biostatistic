--1.1
select distinct socio.poblacion from socio order by socio.poblacion desc;
--1.2
select socio.nombre from socio where socio.poblacion <> 'BURJASSOT' and socio.poblacion <> 'VALENCIA' order by socio.nombre asc;
--1.3
select libro.titulo from libro where libro.titulo like 'LA %' or libro.titulo like '% LA %';
--2.1
select libro.titulo, autor.nombre, libro.anyo from libro join autor on (libro.autor = autor.codigo);
--2.2
(select ejemplar.libro, ejemplar.ejemplar from ejemplar) MINUS (select prestamo.libro, prestamo.ejemplar from prestamo);
--2.3
select distinct libro.titulo, autor.nombre
from prestamo join libro on (prestamo.libro = libro.codigo) join autor on (libro.autor = autor.codigo) 
where prestamo.devolucion is null;
--3.1
select sum(prestados), sum(devueltos), sum(nodevueltos) 
from (select count(prestamo.libro) prestados, prestamo.libro from prestamo group by prestamo.libro) a join 
(select count(prestamo2.libro) devueltos, prestamo2.libro from (select * from prestamo where devolucion is not NULL) prestamo2 group by prestamo2.libro) b on (a.libro=b.libro) join
(select count(prestamo2.libro) nodevueltos, prestamo2.libro from (select * from prestamo where devolucion is NULL) prestamo2 group by prestamo2.libro) c on (b.libro = c.libro);
--3.3
select socio.nombre 
from socio join (select count(prestamo.libro), prestamo.socio from prestamo group by prestamo.socio having count(prestamo.libro) > 5) a 
on (socio.codigo = a.socio) 
where socio.poblacion = 'VALENCIA' order by socio.nombre desc;
--4.1
select count(socio.codigo), socio.poblacion from socio group by socio.poblacion having socio.poblacion = (select max(socio.poblacion) from socio);
--4.2
select a.ejemplar, libro.titulo 
from ((select ejemplar.libro, ejemplar.ejemplar from ejemplar) MINUS (select prestamo.libro, prestamo.ejemplar from prestamo)) a 
join libro on (libro.codigo = a.libro);
