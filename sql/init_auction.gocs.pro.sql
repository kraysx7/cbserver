USE `steamauction`;


-- Таблица узлов-обработчиков (боты и др.)
LOCK TABLES `node` WRITE;
TRUNCATE `node`;
INSERT INTO `node` VALUES 
	(1,  'node1.steamcloud.progame.ru', '127.0.0.1',  20000, '', 0, 0, '', 0), -- dev Reveal Auction
	(30, 'node30.steamcloud.progame.ru', '127.0.0.1', 20000, '', 0, 0, '', 0); -- Reveal Auction
UNLOCK TABLES;


-- Таблица сервисов-типов аукционов, которые предолставляет приложение
LOCK TABLES `service` WRITE;
TRUNCATE `service`;
INSERT INTO `service` VALUES
    (2000, 'Reveal Auction CS:GO'	, 91000, '[ ].', 0, 0, 0); -- Аукцион с закрытой ценой, лоты хранятся в user_service
UNLOCK TABLES;
