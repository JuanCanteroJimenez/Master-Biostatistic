-- phpMyAdmin SQL Dump
-- version 5.0.4
-- https://www.phpmyadmin.net/
--
-- Servidor: localhost
-- Tiempo de generación: 29-11-2021 a las 18:16:55
-- Versión del servidor: 10.3.28-MariaDB
-- Versión de PHP: 7.2.24

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Base de datos: `tienda`
--

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `animales`
--

CREATE TABLE `animales` (
  `tipoanimal` int(11) NOT NULL,
  `nombre` varchar(15) COLLATE latin1_spanish_ci DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_spanish_ci;

--
-- Volcado de datos para la tabla `animales`
--

INSERT INTO `animales` (`tipoanimal`, `nombre`) VALUES
(1, 'Gato'),
(2, 'Perro'),
(3, 'Cerdo'),
(4, 'Oveja'),
(5, 'Caballo'),
(6, 'Hamster'),
(7, 'Loro'),
(8, 'Conejo'),
(9, 'Canario'),
(10, 'Periquito'),
(11, 'Paloma');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `clientes`
--

CREATE TABLE `clientes` (
  `idcliente` int(11) NOT NULL,
  `nombre` varchar(30) COLLATE latin1_spanish_ci DEFAULT NULL,
  `apellido` varchar(30) COLLATE latin1_spanish_ci DEFAULT NULL,
  `email` varchar(30) COLLATE latin1_spanish_ci DEFAULT NULL,
  `direccionpostal` varchar(40) COLLATE latin1_spanish_ci DEFAULT NULL,
  `ciudad` varchar(30) COLLATE latin1_spanish_ci DEFAULT NULL,
  `telefono` varchar(9) COLLATE latin1_spanish_ci DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_spanish_ci;

--
-- Volcado de datos para la tabla `clientes`
--

INSERT INTO `clientes` (`idcliente`, `nombre`, `apellido`, `email`, `direccionpostal`, `ciudad`, `telefono`) VALUES
(1, 'JUAN', 'MARTÍN', 'jm@gmail.com', 'Colon, 12', 'Valencia', '963547873'),
(2, 'JUAN', 'LÓPEZ', 'ml@hotmail.com', 'Ruzafa, 18', 'Valencia', '963454545'),
(3, 'ELENA', 'SALGUERO', 'es@hotmail.com', 'Ruzafa, 13', 'Burjassot', '967343434'),
(4, 'MARÍA', 'DÍAZ', NULL, 'San Vicente, 20', 'Valencia', '964686869'),
(5, 'MARÍA', 'GARCÍA', 'mg@gmail.com', 'Malaga, 53', 'Valencia', '963565656'),
(6, 'FRANCISCO', 'GIMÉNEZ', 'fg@gmail.com', 'Reus, 38', 'Valencia', '639777777'),
(7, 'FEDERICO', 'GIL', NULL, 'Liria, 37', 'Burjassot', '687778789'),
(8, 'CARMEN', 'ALBERO', 'ca@hotmail.com', 'Mayor, 2', 'Burjassot', '656789090'),
(9, 'SARA ', 'ALBERTO', 'sa@gmail.com', 'Menor, 3', 'Liria', '645342434'),
(10, 'PEDRO', 'RUEDA', 'pr@gmail.com', 'Avd. Valencia', 'Liria', '654121234'),
(11, 'ESTHER ', 'DE VES', 'esther.deves@uv.es', 'Almazora, 23', 'Valencia', '654123123'),
(12, 'ALGO', 'VACIO', NULL, NULL, NULL, '632123123'),
(13, 'ALBA', 'ALBA', 'alba@gmail.com', 'nada', 'Valencia', '632254545'),
(14, 'ANA MARÍA', 'FERNÁNDEZ', 'anafe@alumni.uv.es', 'Calle del mediterraneo', 'Betera', '676777888'),
(16, 'NICOLAS JUAN', 'CAMPOS', NULL, NULL, 'Betera', '657877879');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `cursos`
--

CREATE TABLE `cursos` (
  `idcurso` int(11) NOT NULL,
  `titulo` varchar(40) COLLATE latin1_spanish_ci DEFAULT NULL,
  `descripcion` varchar(100) COLLATE latin1_spanish_ci DEFAULT NULL,
  `max_num` int(11) DEFAULT NULL,
  `precio` decimal(7,2) DEFAULT NULL,
  `tipoanimal` int(11) DEFAULT NULL,
  `fechahora` date DEFAULT NULL,
  `lugar` varchar(30) COLLATE latin1_spanish_ci DEFAULT NULL,
  `instructor` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_spanish_ci;

--
-- Volcado de datos para la tabla `cursos`
--

INSERT INTO `cursos` (`idcurso`, `titulo`, `descripcion`, `max_num`, `precio`, `tipoanimal`, `fechahora`, `lugar`, `instructor`) VALUES
(1, 'Educando cachorros', 'Curso para cualquier mascota', 20, '80.00', NULL, '2020-07-16', 'sala 1', 7),
(2, 'Adiestrando grandes perros', 'Curso para evitar perros violentos', 12, '100.00', 2, '2021-08-17', 'sala 2', 1),
(3, 'Higiene pequeños perros', 'Curso para perros pequeños que salen poco de casa', 10, '75.00', 2, '2021-08-16', 'sala 1', 2),
(4, 'Higiene de gatos especiales', 'Curso para ayudar a cuidar a gatos con necesidades especiales', 15, '80.00', 1, '2005-08-17', 'sala 1', 7),
(5, 'Necesidades gatos comunes', 'Curso para aprender a cuidar a un gato común', 10, '60.00', 1, '2006-08-17', 'sala 3', 1),
(6, 'Domesticar a gatos como perros', 'Cómo hacer que los gatos obedezcan.', 15, '60.00', 1, '2007-08-17', 'sala 1', 1),
(7, 'La alimentacion de los conejos', 'Curso para aprender una buena alimentacion de los conejos domesticos', 20, '75.00', 8, '2028-07-17', 'sala 3', 7),
(8, 'Adiestrande grandes perros 2', 'Curso avanzado sobre como manejar los perros violentos', 12, '100.00', 2, '2016-09-17', 'sala 1', 7),
(9, 'Gatos en casa', 'Curso inicial de los cuidados de los gatos', 20, '50.00', 1, '2017-07-17', 'sala 3', NULL),
(10, 'Como tratar una mascota', 'Curso para principiantes', 10, '50.00', 2, '2030-07-18', 'sala 1', 7),
(11, 'Los secretos de los animales domesticos', 'Curso de divulgación general', 10, '35.00', NULL, '2003-09-17', 'sala 2', 2),
(12, 'Dieta sana para GATOS', 'Curso básico', 20, '50.00', 1, '2003-08-18', 'sala 2', 1),
(13, 'Dieta sana para perros', 'Curso básico', 10, '50.00', 2, '2003-07-18', 'sala 1', 1),
(14, 'Dieta sana para perros (segunda edicion)', 'Curso imprescindible', 25, '60.00', 2, '2002-07-18', 'sala 4', 1),
(15, 'Como tratar una mascota genérica', 'Curso básico', 30, '55.00', NULL, '2010-07-18', 'sala 3', NULL),
(16, 'La alimentacion de los periquitos', 'Curso sobe los mejores alpistes', 15, '35.00', 10, '2020-07-18', 'sala 1', 7);

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `cursos_clientes`
--

CREATE TABLE `cursos_clientes` (
  `idcurso` int(11) NOT NULL,
  `idcliente` int(11) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_spanish_ci;

--
-- Volcado de datos para la tabla `cursos_clientes`
--

INSERT INTO `cursos_clientes` (`idcurso`, `idcliente`) VALUES
(1, 1),
(1, 2),
(1, 3),
(1, 7),
(2, 1),
(2, 3),
(2, 4),
(2, 5),
(2, 13),
(3, 1),
(3, 3),
(4, 3),
(4, 7),
(4, 8),
(5, 1),
(5, 2),
(5, 5),
(5, 7),
(5, 8),
(6, 6),
(6, 7),
(6, 8),
(7, 3),
(8, 1),
(8, 2),
(8, 3),
(8, 4),
(8, 5),
(8, 6),
(8, 8),
(9, 9),
(9, 12),
(10, 1),
(10, 2),
(10, 3),
(10, 4),
(10, 5),
(10, 7),
(10, 8),
(10, 10),
(10, 11),
(10, 12),
(11, 1),
(11, 2),
(11, 3),
(13, 1),
(13, 2),
(13, 3),
(13, 4),
(13, 5),
(13, 6);

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `empleados`
--

CREATE TABLE `empleados` (
  `idempleado` int(11) NOT NULL,
  `nombre` varchar(20) COLLATE latin1_spanish_ci DEFAULT NULL,
  `apellidos` varchar(30) COLLATE latin1_spanish_ci DEFAULT NULL,
  `email` varchar(30) COLLATE latin1_spanish_ci DEFAULT NULL,
  `direccion` varchar(30) COLLATE latin1_spanish_ci DEFAULT NULL,
  `telefono` varchar(9) COLLATE latin1_spanish_ci DEFAULT NULL,
  `trabajo` varchar(20) COLLATE latin1_spanish_ci DEFAULT NULL,
  `salario` decimal(7,2) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_spanish_ci;

--
-- Volcado de datos para la tabla `empleados`
--

INSERT INTO `empleados` (`idempleado`, `nombre`, `apellidos`, `email`, `direccion`, `telefono`, `trabajo`, `salario`) VALUES
(1, 'Juan', 'Díaz', 'jdiaz@domes.es', 'Colon, 3, Valencia', '963676767', 'instructor', '1300.00'),
(2, 'Miguel', 'Pardo', 'mpardo@domes.es', 'Jativa, 13, Valencia', '963545454', 'instructor', '1400.00'),
(3, 'Miguel', 'Pérez', 'mperez@domes.es', 'Guillem, 22, Valencia', '963543323', 'veterinario', '2300.00'),
(4, 'Alicia', 'San Juan', 'asanjuan@domes.es', 'Mayor, 3, Burjassot', '962656555', 'veterinario', '2400.00'),
(5, 'Fernando', 'Saez', 'fsaez@domes.es', 'Moliner, 5, Burjassot', '962676667', 'comercial', '950.00'),
(6, 'Jose', 'García', 'jgarcia@domes.es', 'Poeta Artola, 7, Valencia', '963456546', 'comercial', '1200.00'),
(7, 'Juan', 'Martínez', 'jmartinez@domes.es', 'Moliner,7, Burjassot', '962878787', 'instructor', '1500.00'),
(8, 'Clara', 'Fernández', 'clfernandez@gmail.com', 'Corredera, 8, Liria', '639776565', 'comercial', '1400.00'),
(9, 'Isabel ', 'Cuenca', 'icuenca@hotmail.com', 'Corredera, 38, Valencia', '967656566', 'administrativo', '1350.00'),
(10, 'Juana', 'López', 'juanlo@gmail.com', 'Moliner, 3, Burjassot', '646897678', 'comercial', '1450.00');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `mascotas`
--

CREATE TABLE `mascotas` (
  `idmascota` int(11) NOT NULL,
  `idcliente` int(11) DEFAULT NULL,
  `nombremascota` varchar(20) COLLATE latin1_spanish_ci DEFAULT NULL,
  `tipoanimal` int(11) DEFAULT NULL,
  `fechanac` date DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_spanish_ci;

--
-- Volcado de datos para la tabla `mascotas`
--

INSERT INTO `mascotas` (`idmascota`, `idcliente`, `nombremascota`, `tipoanimal`, `fechanac`) VALUES
(1, 1, 'Pepito', 1, '2026-03-08'),
(2, 2, 'Mara', 2, '2022-02-07'),
(3, 1, 'Flipo', 1, '2022-05-10'),
(4, 2, 'Flipa', 4, '2008-02-11'),
(5, 3, 'Misia', 3, '2007-03-11'),
(6, 4, 'Saturno', 5, '2001-01-11'),
(7, 5, 'Mansito', 2, '2003-05-08'),
(8, 7, 'Fortachon', 10, '2005-05-08'),
(9, 8, 'Fresita', 1, '2005-06-09');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `pedidos`
--

CREATE TABLE `pedidos` (
  `idpedido` int(11) NOT NULL,
  `empleadoventa` int(11) DEFAULT NULL,
  `idcliente` int(11) DEFAULT NULL,
  `fechapedido` date DEFAULT NULL,
  `fechaentrega` date DEFAULT NULL,
  `direccionentrega` varchar(40) COLLATE latin1_spanish_ci DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_spanish_ci;

--
-- Volcado de datos para la tabla `pedidos`
--

INSERT INTO `pedidos` (`idpedido`, `empleadoventa`, `idcliente`, `fechapedido`, `fechaentrega`, `direccionentrega`) VALUES
(1, 5, 1, '2025-03-16', '2019-04-16', 'C/ Colon, 3, Valencia'),
(2, 6, 2, '2028-04-16', '2023-05-16', 'C/ Jativa, 10, Valencia'),
(3, 10, 1, '2023-03-17', '2017-04-17', 'C/ Reus, 3, Valencia'),
(4, 10, 2, '2005-05-17', '2030-05-17', 'Avd. Burjassot, 10, Valencia'),
(5, 10, 3, '2011-05-17', '2005-06-17', 'Avd. Constitucion, 14, Valencia'),
(6, 5, 4, '2021-05-17', '2015-06-17', 'Avd. Constitucion, 20, Valencia'),
(7, 5, 5, '2031-05-17', '2025-06-17', 'C/ Malaga, 3, Valencia'),
(8, 6, 5, '2009-06-16', '2004-07-16', 'C/ Poeta Artola, 2, Valencia'),
(9, 10, 1, '2020-04-17', '2015-05-17', 'C/ Poeta Querol, 3, Valencia'),
(10, 10, 1, '2031-01-17', '2025-02-17', 'C/ Miguel Delibes, 4, Burjassot'),
(11, 10, 5, '2025-02-17', '2022-03-17', 'C/ Malaga, 3, Valencia'),
(12, 10, 5, '2027-02-17', '2024-03-17', 'C/Malaga, 3, Valencia'),
(13, 6, 4, '2010-09-17', NULL, 'Avda. Universitat s/n'),
(14, 5, 2, '2007-01-17', '2001-02-17', 'Avd. Burjassot, 10, Valencia'),
(15, 5, 1, '2005-10-17', NULL, 'C/ Colon, 3, Valencia'),
(16, 6, 5, '2004-04-17', '2029-04-17', 'C/ Malaga, 3, Valencia');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `pedidos_productos`
--

CREATE TABLE `pedidos_productos` (
  `idpedido` int(11) NOT NULL,
  `idproducto` int(11) NOT NULL,
  `cantidad` int(11) DEFAULT NULL,
  `precioreal` decimal(7,2) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_spanish_ci;

--
-- Volcado de datos para la tabla `pedidos_productos`
--

INSERT INTO `pedidos_productos` (`idpedido`, `idproducto`, `cantidad`, `precioreal`) VALUES
(1, 1, 3, '13.41'),
(2, 3, 1, '28.17'),
(2, 4, 3, '17.01'),
(2, 5, 1, '13.50'),
(3, 6, 4, '11.07'),
(4, 7, 5, '8.37'),
(5, 1, 3, '13.41'),
(5, 2, 3, '8.01'),
(5, 5, 4, '13.50'),
(5, 6, 2, '11.07'),
(6, 7, 8, '8.37'),
(7, 3, 2, '28.17'),
(8, 1, 4, '13.41'),
(9, 2, 3, '8.01'),
(10, 1, 2, '13.41'),
(10, 2, 3, '8.01'),
(11, 1, 4, '13.41'),
(12, 5, 1, '13.50'),
(13, 1, 5, '13.50'),
(13, 2, 6, '8.03'),
(13, 4, 10, '11.08'),
(14, 2, 11, '8.00'),
(15, 1, 3, '13.30'),
(15, 6, 10, '11.07'),
(15, 7, 3, '8.01'),
(16, 2, 10, '7.99'),
(16, 3, 5, '28.00'),
(16, 4, 3, '17.02');

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `productos`
--

CREATE TABLE `productos` (
  `idproducto` int(11) NOT NULL,
  `descripcion` varchar(40) COLLATE latin1_spanish_ci DEFAULT NULL,
  `precio_compra` decimal(7,2) DEFAULT NULL,
  `precio_venta` decimal(7,2) DEFAULT NULL,
  `stock` int(11) DEFAULT NULL,
  `idproveedor` int(11) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_spanish_ci;

--
-- Volcado de datos para la tabla `productos`
--

INSERT INTO `productos` (`idproducto`, `descripcion`, `precio_compra`, `precio_venta`, `stock`, `idproveedor`) VALUES
(1, 'Comida Gatos, 5kg', '10.80', '14.90', 50, 1),
(2, 'Comida Periquitos, 1Kg', '6.30', '8.90', 40, 1),
(3, 'Comida Perros medianos completa, 5 kg', '22.30', '31.30', 20, 1),
(4, 'Comida Loros, 2 Kg', '10.90', '18.90', 30, 2),
(5, 'Comida Conejos, salud, 5 Kg', '10.80', '15.00', 10, 2),
(6, 'Amoxicilina, 10 mg', '8.00', '12.30', 6, 3),
(7, 'Parocetamol 200 mg, soluble', '7.20', '9.30', 10, 3),
(8, 'Champu canino 1000 ml', '12.30', '17.80', 6, 3),
(9, 'Champu especial gatos, 500 ml', '8.30', '13.50', 8, 3),
(10, 'Limpieza en seco gatos, 300 ml', '7.90', '10.30', 22, 3),
(11, 'Trasportador perros, grande', '33.90', '60.00', 8, 1),
(12, 'Trasportador perros, mediano', '23.80', '43.20', 6, 1),
(13, 'Trasportador gatos, cachorros', '23.80', '32.30', 4, 1),
(14, 'Trasportador gatos, adultos', '34.80', '49.30', 8, 2),
(15, 'Cesta pequeña, gatos y perros', '23.30', '34.80', 5, 2),
(16, 'Cesta grande, gatos y perros', '28.30', '37.80', 7, 2),
(20, 'Comida Perros grandes, 10 kg', '23.40', '32.10', 20, 1);

-- --------------------------------------------------------

--
-- Estructura de tabla para la tabla `proveedores`
--

CREATE TABLE `proveedores` (
  `idproveedor` int(11) NOT NULL,
  `nombre` varchar(30) COLLATE latin1_spanish_ci DEFAULT NULL,
  `notas` varchar(50) COLLATE latin1_spanish_ci DEFAULT NULL,
  `email` varchar(20) COLLATE latin1_spanish_ci DEFAULT NULL,
  `telefono` varchar(9) COLLATE latin1_spanish_ci DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1 COLLATE=latin1_spanish_ci;

--
-- Volcado de datos para la tabla `proveedores`
--

INSERT INTO `proveedores` (`idproveedor`, `nombre`, `notas`, `email`, `telefono`) VALUES
(1, 'ProdAnimal, S. L.', NULL, 'pa@gmail.com', '967656565'),
(2, 'ProVeter, S.L', NULL, 'pv@hotmail.com', '963456554'),
(3, 'Cuidados Animales, S.L', NULL, 'ca@hotmail.com', '965434343'),
(4, 'ProductZoo, S.L.', NULL, 'elgato@gmail.com', '963787878'),
(5, 'VentaParaAnimales, S. A', NULL, NULL, '639706865'),
(6, 'Mascotines, S.L.', 'Muy rápidos en su servicio', 'mascotines@gmail.com', NULL);

--
-- Índices para tablas volcadas
--

--
-- Indices de la tabla `animales`
--
ALTER TABLE `animales`
  ADD PRIMARY KEY (`tipoanimal`);

--
-- Indices de la tabla `clientes`
--
ALTER TABLE `clientes`
  ADD PRIMARY KEY (`idcliente`);

--
-- Indices de la tabla `cursos`
--
ALTER TABLE `cursos`
  ADD PRIMARY KEY (`idcurso`),
  ADD KEY `tipoanimal` (`tipoanimal`),
  ADD KEY `instructor` (`instructor`);

--
-- Indices de la tabla `cursos_clientes`
--
ALTER TABLE `cursos_clientes`
  ADD PRIMARY KEY (`idcurso`,`idcliente`),
  ADD KEY `idcliente` (`idcliente`);

--
-- Indices de la tabla `empleados`
--
ALTER TABLE `empleados`
  ADD PRIMARY KEY (`idempleado`);

--
-- Indices de la tabla `mascotas`
--
ALTER TABLE `mascotas`
  ADD PRIMARY KEY (`idmascota`),
  ADD KEY `idcliente` (`idcliente`),
  ADD KEY `tipoanimal` (`tipoanimal`);

--
-- Indices de la tabla `pedidos`
--
ALTER TABLE `pedidos`
  ADD PRIMARY KEY (`idpedido`),
  ADD KEY `empleadoventa` (`empleadoventa`),
  ADD KEY `idcliente` (`idcliente`);

--
-- Indices de la tabla `pedidos_productos`
--
ALTER TABLE `pedidos_productos`
  ADD PRIMARY KEY (`idpedido`,`idproducto`),
  ADD KEY `idproducto` (`idproducto`);

--
-- Indices de la tabla `productos`
--
ALTER TABLE `productos`
  ADD PRIMARY KEY (`idproducto`),
  ADD KEY `idproveedor` (`idproveedor`);

--
-- Indices de la tabla `proveedores`
--
ALTER TABLE `proveedores`
  ADD PRIMARY KEY (`idproveedor`);

--
-- Restricciones para tablas volcadas
--

--
-- Filtros para la tabla `cursos`
--
ALTER TABLE `cursos`
  ADD CONSTRAINT `cursos_ibfk_1` FOREIGN KEY (`tipoanimal`) REFERENCES `animales` (`tipoanimal`) ON DELETE CASCADE,
  ADD CONSTRAINT `cursos_ibfk_2` FOREIGN KEY (`instructor`) REFERENCES `empleados` (`idempleado`) ON DELETE CASCADE;

--
-- Filtros para la tabla `cursos_clientes`
--
ALTER TABLE `cursos_clientes`
  ADD CONSTRAINT `cursos_clientes_ibfk_1` FOREIGN KEY (`idcurso`) REFERENCES `cursos` (`idcurso`) ON DELETE CASCADE,
  ADD CONSTRAINT `cursos_clientes_ibfk_2` FOREIGN KEY (`idcliente`) REFERENCES `clientes` (`idcliente`) ON DELETE CASCADE;

--
-- Filtros para la tabla `mascotas`
--
ALTER TABLE `mascotas`
  ADD CONSTRAINT `mascotas_ibfk_1` FOREIGN KEY (`idcliente`) REFERENCES `clientes` (`idcliente`) ON DELETE CASCADE,
  ADD CONSTRAINT `mascotas_ibfk_2` FOREIGN KEY (`tipoanimal`) REFERENCES `animales` (`tipoanimal`) ON DELETE CASCADE;

--
-- Filtros para la tabla `pedidos`
--
ALTER TABLE `pedidos`
  ADD CONSTRAINT `pedidos_ibfk_1` FOREIGN KEY (`empleadoventa`) REFERENCES `empleados` (`idempleado`) ON DELETE CASCADE,
  ADD CONSTRAINT `pedidos_ibfk_2` FOREIGN KEY (`idcliente`) REFERENCES `clientes` (`idcliente`) ON DELETE CASCADE;

--
-- Filtros para la tabla `pedidos_productos`
--
ALTER TABLE `pedidos_productos`
  ADD CONSTRAINT `pedidos_productos_ibfk_1` FOREIGN KEY (`idpedido`) REFERENCES `pedidos` (`idpedido`) ON DELETE CASCADE,
  ADD CONSTRAINT `pedidos_productos_ibfk_2` FOREIGN KEY (`idproducto`) REFERENCES `productos` (`idproducto`) ON DELETE CASCADE;

--
-- Filtros para la tabla `productos`
--
ALTER TABLE `productos`
  ADD CONSTRAINT `productos_ibfk_1` FOREIGN KEY (`idproveedor`) REFERENCES `proveedores` (`idproveedor`) ON DELETE CASCADE;
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
