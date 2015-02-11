DROP DATABASE IF EXISTS `cloudbilling`;

CREATE DATABASE `cloudbilling` DEFAULT CHARSET=utf8;

USE `cloudbilling`;

-- --------------------------------------------------------
-- Таблица для хранения информации о пользователях
-- --------------------------------------------------------
DROP TABLE IF EXISTS `user`;
CREATE TABLE `user` (
    `user_id`       BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY, -- идентификатор пользователя
    `name`          VARCHAR(128) NOT NULL,
	`balance` 		INT 	    NOT NULL DEFAULT 0,	
	`ban` 			TINYINT		DEFAULT 0,		    -- флаг пользователя о бане. если > 0, то юзер заблокирован. значение - причина бана.
	`ban_comment`	VARCHAR(64) DEFAULT NULL,	    -- ещё более подробное объяснение бана (устанавливается администратором)
	`password`		CHAR(40) 	NOT NULL, 	        -- md5(...)
	`reg_date`		DATETIME 	DEFAULT NULL,
	`email` 		VARCHAR(30) DEFAULT NULL,

	`last_login` 	DATETIME 	DEFAULT NULL,
	`last_logout` 	DATETIME 	DEFAULT NULL,
	`last_ip`		INT 	 	DEFAULT 0,
	`comment`		VARCHAR(64) DEFAULT NULL	    -- коментарий к пользователю
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;

-- --------------------------------------------------------
-- Таблица для хранения информации о сервисах, подключенных пользователю
-- --------------------------------------------------------
DROP TABLE IF EXISTS `user_service`;
CREATE TABLE `user_service` (
    `user_service_id`   BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,-- порядковый номер подключенного сервиса
    `user_id`           BIGINT NOT NULL,            -- идентификатор пользователя
    `service_id`        INT NOT NULL,               -- идентификатор сервиса
    `node_id`           INT NOT NULL,               -- какой из узлов предоставляет сервис
    `params`            VARCHAR(128) NOT NULL,      -- описание параметров подключенного сервиса. формат : "[{key1, val}, {key2, val}, ...]"
    `end_date`          DATETIME,                   -- когда отключить сервис. NULL - без срока действия
    `status`            TINYINT NOT NULL DEFAULT 0  -- статус*
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;

-- * 0 - не активирован, 1 - поступил в обработку, 2 - активирован, 3 - активирован+автопродление
-- * 5 - ошибка активации, пользователь забанен,
-- * 6 - ошибка активации, подключаемый сервис закрыт (недоступен для подключения)
-- * 7 - ошибка активации, нет доступных ресурсов для подключения сервиса

-- --------------------------------------------------------
-- Таблица для хранения информации о сервисах, которые может подключить пользователь
-- --------------------------------------------------------
DROP TABLE IF EXISTS `service`;
CREATE TABLE `service` (
    `service_id`        INT NOT NULL PRIMARY KEY,   -- идентификатор сервиса
    `name`              VARCHAR(128) NOT NULL,      -- название (напр. VPS Lite, VPS Medium, VPS Super, VPS Master \ Hosting Lite, Hosting ...)
    `type`              INT NOT NULL,               -- тип сервиса
    `params`            VARCHAR(128) NOT NULL,      -- описание параметров сервиса. формат : "[{key1, val}, {key2, val}, {key3, val}, ...]"
    `cost`              INT NOT NULL DEFAULT 0,     -- стоимость использования за один расчётный период (указывается в копейках)
    `period`            INT NOT NULL DEFAULT 0,     -- расчётный период в минутах. Как правило расчётный период - 1 день (1440 минут)
    `status`            TINYINT NOT NULL            -- 0 - сервис открыт, 1 - сервис закрыт
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;

-- 1Tb = 1024Gb =1048576Mb
-- 2Tb = 2048Gb =2097152Mb

-- Типы сервисов :                        | Параметры (пример) :
-- 0x00000001 - OpenPanel hosting Fixed   | [{vhosts_count, 5}, {db_count, 5}, {hdd, 2048}]
-- 0x00000002 - OpenPanel hosting Scale   | [{vhost_cost, 10}, {db_cost, 10}, {hdd_cost, 10}]
-- 0x00000004 - cPanel hosting Fixed      | [{vhost_cost, 10}, {db_cost, 10}, {hdd_cost, 10}]

-- 0x00000100 - OpenNebula VPS/KVM Fixed  | [{cpus, 5}, {memory, 5}, {hdd, 10240}, {traffic, 1024000}]
-- 0x00000200 - OpenNebula VPS/KVM Scale  | 

-- 0x00001000 - TCP-Proxy network filter
-- 0x00002000 - Web-Proxy network filter

-- 0x01000000 - Radio (WiFi)  internet access
-- 0x02000000 - Cable (RJ-45) internet access



-- hosting_service.params (квота)
-- Количество БД, Количество Хостов, Размер дисковой квоты

-- --------------------------------------------------------
-- Таблица описывающая узлы кластера, обслуживающих различные сервисы
-- --------------------------------------------------------
DROP TABLE IF EXISTS `node`;
CREATE TABLE `node` (
    `node_id`           INT NOT NULL,               -- номер узла
	`hostname`          VARCHAR(128) NOT NULL,      -- имя хоста для данного узла
	`ip`                VARCHAR(64) NOT NULL,       -- ip адрес узла
    `service_type` INT NOT NULL,                    -- какой тип сервиса обслуживает узел
    `status`            TINYINT NOT NULL            -- 0 - узел открыт, 1 - узел закрыт
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;



-- --------------------------------------------------------
-- Таблица для хранения информации о заказах
-- --------------------------------------------------------
DROP TABLE IF EXISTS `order`;
CREATE TABLE `order` (
    `order_id`    INT NOT NULL AUTO_INCREMENT PRIMARY KEY,  -- номер заказа
    `user_id`	  BIGINT NOT NULL,		    	        -- идентификатор пользователя - `чей заказ`
    `service_id`  INT NOT NULL,                     -- идентификатор подключаемого сервиса
    `params`      VARCHAR(128) NOT NULL,
    `period`      INT NOT NULL DEFAULT 0,           -- оплачиваемый период действия (в минутах)
    `status`	  TINYINT NOT NULL DEFAULT 0,       -- статус заказа*
    `processing_mode` TINYINT NOT NULL DEFAULT 0,   -- режим обработки (0 - автоматически, 1 - по факту оплаты, 2 - ручник)
    `create_date` DATETIME,
    `close_date`  DATETIME
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;


-- * 0 - не обработан, 1 - поступил в обработку, 2 - обработан
-- * 3 - обработан, но денег для подключения сервиса нет




-- Транзакция - в общем случае, любая операция с использованием личного счёта. 

-- --------------------------------------------------------
-- Таблица для хранения информации о транзакциях биллинг системы
-- --------------------------------------------------------

DROP TABLE IF EXISTS `transaction`;
CREATE TABLE `transaction` (
	`transaction_id`		BIGINT 			NOT NULL AUTO_INCREMENT PRIMARY KEY,
    `type`                  INT             NOT NULL,           -- тип. [0 - оплата включенного сервиса, 1 - оплата заказа]
    `param`                 BIGINT          NOT NULL,           -- параметр транзакции
	`user_id`			    BIGINT 			NOT NULL,
	`cost`				    INT 			NOT NULL DEFAULT 0, -- сумма транзакции (сколько снимаем или пополняем)
	`new_balance` 			INT 			NOT NULL DEFAULT 0,	-- баланс ПОСЛЕ выполнения транзакции
	`status` 			    TINYINT			NOT NULL DEFAULT 0,	-- статус транзакции. 0 - создана. >0 - закрыта. 1 = успешно. 2,3,4,5... = коды ошибок
	`date_open` 			DATETIME		DEFAULT NULL,
	`date_close` 			DATETIME		DEFAULT NULL
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;



-- ######################## ХРАНИМЫЕ ПРОЦЕДУРЫ ######################## 
DELIMITER //

DROP PROCEDURE IF EXISTS `create_transaction`; //
CREATE  PROCEDURE `create_transaction`(IN `in_type` INT, IN `in_param` BIGINT, IN `in_user_id` BIGINT, IN `in_cost` INT, IN `in_new_balance` INT, IN `in_status` TINYINT, IN `in_date_open` DATETIME, IN `in_date_close` DATETIME)
BEGIN
	INSERT INTO `transaction` VALUES (NULL, `in_type`, `in_param`, `in_user_id`, `in_cost`, `in_new_balance`, `in_status`, `in_date_open`, `in_date_close`);
	SELECT LAST_INSERT_ID() AS `transaction_id`;
END; //


DROP PROCEDURE IF EXISTS `create_order`; //
CREATE  PROCEDURE `create_order`(IN `in_user_id` INT, IN `in_service_id` INT, IN `in_params` VARCHAR(128), IN `in_period` INT, IN `in_status` INT, IN `in_processing_mode` INT, IN `in_create_date` DATETIME, IN `in_close_date` DATETIME)
BEGIN
    INSERT INTO `order` VALUES (NULL,`in_user_id`,`in_service_id`, `in_params`, `in_period`,`in_status`,`in_processing_mode`,`in_create_date`,`in_close_date`);
	SELECT LAST_INSERT_ID() AS `order_id`;
END; //


DROP PROCEDURE IF EXISTS `add_user_service`; //
CREATE  PROCEDURE `add_user_service`(IN `in_user_id` BIGINT, IN `in_service_id` INT, IN `in_node_id` INT, IN `in_params` VARCHAR(128), IN `in_end_date` DATETIME, IN `in_status` TINYINT)
BEGIN
	INSERT INTO `user_service` VALUES (NULL, `in_user_id`, `in_service_id`, `in_node_id`, `in_params`, `in_end_date`, `in_status`);
	SELECT LAST_INSERT_ID() AS `user_service_id`;
END; //


-- ######################## DEBUG INSERTS ######################## 


LOCK TABLES `user` WRITE;
INSERT INTO `user` VALUES (1,'kray',1000000,0,NULL,'123',NULL,'qwe',NULL,NULL,0,NULL);
UNLOCK TABLES;

-- Таблица сервисов
LOCK TABLES `service` WRITE;
INSERT INTO `service` VALUES
    (1,'Hosting Test'  ,1,'[{vhosts,1}, {dbcount,0},{hdd, 100}  ].',0,0,0),
    (2,'Hosting Lite'  ,1,'[{vhosts,1}, {dbcount,1},{hdd, 1000} ].',100,1440,0),
    (3,'Hosting Medium',1,'[{vhosts,5}, {dbcount,2},{hdd, 5000} ].',200,1440,0),
    (4,'Hosting Hard'  ,1,'[{vhosts,10},{dbcount,5},{hdd, 10000}].',300,1440,0);
UNLOCK TABLES;

-- LOCK TABLES `user_service` WRITE;
-- INSERT INTO `user_service` VALUES 
--    (NULL,1,1,1,'[].',NULL,1),
--    (NULL,1,2,1,'[].',NULL,1),
--    (NULL,1,3,2,'[].',NULL,1);
-- UNLOCK TABLES;

LOCK TABLES `node` WRITE;
INSERT INTO `node` VALUES 
    (1, 'vh1.main.clooud.ru', '192.168.8.209', 1, 0);
UNLOCK TABLES;







