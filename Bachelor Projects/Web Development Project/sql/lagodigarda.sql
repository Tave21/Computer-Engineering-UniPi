-- phpMyAdmin SQL Dump
-- version 5.1.1
-- https://www.phpmyadmin.net/
--
-- Host: 127.0.0.1
-- Creato il: Giu 13, 2022 alle 15:01
-- Versione del server: 5.7.28
-- Versione PHP: 8.0.10

SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO";
START TRANSACTION;
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;

--
-- Database: `lagodigarda`
--
CREATE DATABASE IF NOT EXISTS `lagodigarda` DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci;
USE `lagodigarda`;

DELIMITER $$
--
-- Procedure
--
CREATE DEFINER=`root`@`localhost` PROCEDURE `Inserisci_File` (IN `n` VARCHAR(100), IN `o` VARCHAR(100), IN `e` VARCHAR(20), IN `d` INT(100) UNSIGNED, IN `us` VARCHAR(100))  BEGIN
	INSERT INTO file 
    VALUES ( n ,o , e ,d,us);
END$$

DELIMITER ;

-- --------------------------------------------------------

--
-- Struttura della tabella `accesso`
--

CREATE TABLE `accesso` (
  `username` varchar(50) NOT NULL,
  `pass` varchar(100) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

--
-- Dump dei dati per la tabella `accesso`
--

INSERT INTO `accesso` (`username`, `pass`) VALUES
('admin', '$2y$10$NjgLwkDw1qgtCOrqTXi1U.Cg.uB.7dA2F1PL19wI0EORn1pqZGyLG'),
('Eleonora', '$2y$10$ahjT6O1BMCccO1IGpwl/WOjtEXD3/pF2HVAYZ/kMSvG3hICQgfJKe'),
('tave', '$2y$10$ALYk6DyEj.9HQAgVOBhR8.rzGB3sbiZHfDg.1Po1xDUIt9onrOcxK');

--
-- Trigger `accesso`
--
DELIMITER $$
CREATE TRIGGER `Elimina_tutto` BEFORE DELETE ON `accesso` FOR EACH ROW BEGIN
DELETE FROM file WHERE username = OLD.username;
DELETE FROM messaggio WHERE user = OLD.username;
DELETE FROM newsletter WHERE user = OLD.username;
DELETE FROM prenotazione WHERE user = OLD.username;
END
$$
DELIMITER ;

-- --------------------------------------------------------

--
-- Struttura della tabella `file`
--

CREATE TABLE `file` (
  `nome_memoria` varchar(100) NOT NULL COMMENT 'Nome usato per la memorizzazione nel server applicativo',
  `nome_effettivo` varchar(100) NOT NULL COMMENT 'Nome dato dal Mittente',
  `estensione` varchar(20) NOT NULL,
  `Dimensione` int(100) UNSIGNED NOT NULL DEFAULT '0' COMMENT 'Espressa in KB',
  `username` varchar(100) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

--
-- Dump dei dati per la tabella `file`
--

INSERT INTO `file` (`nome_memoria`, `nome_effettivo`, `estensione`, `Dimensione`, `username`) VALUES
('05-scalinata-busatte-tempesta-lago-di-garda-360gardalife-1200x7990611202212131006100.jpg', '05-scalinata-busatte-tempesta-lago-di-garda-360gardalife-1200x799.jpg', 'image/jpeg', 177, 'tave'),
('100000222236911813869270611202212112206220.jpeg', '10000022223691181386927.jpeg', 'image/jpeg', 275, 'Eleonora'),
('giro del lago di garda in macchina itinerario0611202212131006101.jpeg', 'giro del lago di garda in macchina itinerario.jpeg', 'image/jpeg', 196, 'tave'),
('italy-lake-garda0611202212134406440.jpg', 'italy-lake-garda.jpg', 'image/jpeg', 52, 'tave'),
('limone20611202212112206221.jpg', 'limone2.jpg', 'image/jpeg', 138, 'Eleonora'),
('vista-lago-di-garda0611202212112206222.jpg', 'vista-lago-di-garda.jpg', 'image/jpeg', 199, 'Eleonora'),
('why-lake-garda0611202212112206223.jpg', 'why-lake-garda.jpg', 'image/jpeg', 364, 'Eleonora');

-- --------------------------------------------------------

--
-- Struttura della tabella `messaggio`
--

CREATE TABLE `messaggio` (
  `id` int(11) NOT NULL,
  `messaggio` text NOT NULL,
  `data` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `user` varchar(100) NOT NULL,
  `luogo` varchar(100) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

--
-- Dump dei dati per la tabella `messaggio`
--

INSERT INTO `messaggio` (`id`, `messaggio`, `data`, `user`, `luogo`) VALUES
(1, 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed eleifend tellus non eros maximus, sit amet consectetur sapien fermentum. Etiam venenatis et sem non dapibus. Aenean consectetur massa et laoreet dapibus. Fusce tempor purus vitae felis gravida, ac feugiat metus congue. Mauris dolor nulla, pretium pretium dolor eu, mattis maximus est. Mauris fermentum euismod gravida. Cras scelerisque sed velit volutpat dignissim. Sed vehicula sit amet quam ac lacinia. Quisque pulvinar finibus risus, ac vestibulum tellus tincidunt malesuada. Duis hendrerit iaculis dui non congue. Sed blandit vel nibh sed tincidunt. Ut eu placerat orci. Mauris non laoreet urna. Donec nec viverra felis. Suspendisse libero mauris, tempor a sodales vitae, facilisis non lorem.\r\n\r\nPraesent eu facilisis metus. Nulla facilisi. Donec dictum porta ornare. Donec tristique lorem id purus interdum, non blandit nunc viverra. Curabitur non pharetra dolor. Nulla cursus consequat ornare. Integer molestie, tortor vel luctus pretium, lacus quam sagittis nibh, ac feugiat nisi tortor ut metus. Praesent pretium scelerisque blandit. Ut nisi augue, sagittis at cursus ut, congue pharetra lacus. Integer viverra nunc felis, eget ultrices leo sodales et. Cras porttitor, nunc id tincidunt tempor, lorem nulla rutrum nisi, ut egestas eros leo non velit.', '2022-06-11 11:03:02', 'tave', 'Grotte di Catullo'),
(2, 'Nam egestas faucibus suscipit. Curabitur ac augue vitae justo venenatis efficitur. Ut justo turpis, auctor nec purus vitae, condimentum interdum risus. Vestibulum sem arcu, finibus id molestie sed, luctus a purus. Ut ut mauris nec eros ornare convallis. Duis vitae commodo justo. Sed nunc ante, pharetra id risus a, finibus facilisis risus.\r\n\r\nProin aliquet rhoncus metus, a ultrices justo scelerisque quis. Quisque imperdiet tempus sodales. Cras id pharetra nibh. Cras eu odio diam. Maecenas dictum varius magna in ornare. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nunc tristique, arcu et luctus ornare, ipsum libero fringilla augue, eget efficitur est urna vitae risus. Nulla rutrum euismod sollicitudin. Sed aliquam justo quis turpis porttitor elementum. Praesent malesuada diam lectus.', '2022-06-11 11:03:25', 'tave', 'Castello Scaligero'),
(3, 'Praesent eu facilisis metus. Nulla facilisi. Donec dictum porta ornare. Donec tristique lorem id purus interdum, non blandit nunc viverra. Curabitur non pharetra dolor. Nulla cursus consequat ornare. Integer molestie, tortor vel luctus pretium, lacus quam sagittis nibh, ac feugiat nisi tortor ut metus. Praesent pretium scelerisque blandit. Ut nisi augue, sagittis at cursus ut, congue pharetra lacus. Integer viverra nunc felis, eget ultrices leo sodales et. Cras porttitor, nunc id tincidunt tempor, lorem nulla rutrum nisi, ut egestas eros leo non velit.', '2022-06-11 11:03:51', 'Eleonora', 'Castello Malcesine'),
(4, 'Ut cursus maximus dolor, ac sagittis dolor. Quisque blandit velit id commodo dapibus. Vivamus tincidunt eget eros a tincidunt. Nullam vel massa non ex tincidunt viverra sed ac sem. Morbi eu purus tincidunt, porttitor lacus eu, sodales tortor. Sed tempus tincidunt elementum. Nulla varius eros nibh, at luctus massa bibendum at. Mauris ut turpis a urna euismod pellentesque vel non lectus. Nunc at est in purus imperdiet tincidunt. Morbi dictum sem a turpis ultrices venenatis. Donec scelerisque orci eros, sit amet consequat felis gravida lobortis.', '2022-06-11 11:04:05', 'Eleonora', 'Vittoriale degli Italiani');

-- --------------------------------------------------------

--
-- Struttura della tabella `newsletter`
--

CREATE TABLE `newsletter` (
  `nome` varchar(100) NOT NULL,
  `cognome` varchar(100) NOT NULL,
  `cellulare` varchar(15) NOT NULL,
  `email` varchar(100) NOT NULL,
  `citta` varchar(100) NOT NULL,
  `user` varchar(100) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

--
-- Dump dei dati per la tabella `newsletter`
--

INSERT INTO `newsletter` (`nome`, `cognome`, `cellulare`, `email`, `citta`, `user`) VALUES
('Giuseppe', 'Rossi', '3384452771', 'giusepperossi@gmail.com', 'Massa', ''),
('Francesco', 'Taverna', '3450921888', 'iltave4ever@libero.it', 'Sarzana', ''),
('Francesco', 'Taverna', '3384452771', 'iltaver@libero.it', 'Sarzana', 'tave');

-- --------------------------------------------------------

--
-- Struttura della tabella `prenotazione`
--

CREATE TABLE `prenotazione` (
  `Nome` varchar(100) NOT NULL,
  `Cognome` varchar(100) NOT NULL,
  `data` date NOT NULL,
  `numero_persone` int(11) NOT NULL,
  `visita` varchar(100) NOT NULL,
  `richieste` text NOT NULL,
  `id` int(11) NOT NULL,
  `costo` int(11) NOT NULL,
  `user` varchar(100) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

--
-- Dump dei dati per la tabella `prenotazione`
--

INSERT INTO `prenotazione` (`Nome`, `Cognome`, `data`, `numero_persone`, `visita`, `richieste`, `id`, `costo`, `user`) VALUES
('Eleonora', 'Sgorbini', '2022-06-30', 2, 'Visita Castello Malcesine', 'nessuna', 87, 5, 'Eleonora'),
('Eleonora', 'Sgorbini', '2022-06-30', 3, 'Visita Grotte Catullo', 'Vorrei portare il cane', 88, 15, 'Eleonora');

--
-- Indici per le tabelle scaricate
--

--
-- Indici per le tabelle `accesso`
--
ALTER TABLE `accesso`
  ADD PRIMARY KEY (`username`);

--
-- Indici per le tabelle `file`
--
ALTER TABLE `file`
  ADD PRIMARY KEY (`nome_memoria`);

--
-- Indici per le tabelle `messaggio`
--
ALTER TABLE `messaggio`
  ADD PRIMARY KEY (`id`),
  ADD KEY `usermess` (`user`);

--
-- Indici per le tabelle `newsletter`
--
ALTER TABLE `newsletter`
  ADD PRIMARY KEY (`email`);

--
-- Indici per le tabelle `prenotazione`
--
ALTER TABLE `prenotazione`
  ADD PRIMARY KEY (`id`),
  ADD KEY `use` (`user`);

--
-- AUTO_INCREMENT per le tabelle scaricate
--

--
-- AUTO_INCREMENT per la tabella `messaggio`
--
ALTER TABLE `messaggio`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=5;

--
-- AUTO_INCREMENT per la tabella `prenotazione`
--
ALTER TABLE `prenotazione`
  MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=89;
COMMIT;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;