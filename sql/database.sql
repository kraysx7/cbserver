DROP DATABASE IF EXISTS `tmskinscom`;
CREATE DATABASE `tmskinscom` DEFAULT CHARSET=utf8;
-- USE `progameru`;
USE `tmskinscom`;

-- --------------------------------------------------------
-- Таблица для хранения информации о пользователях
-- --------------------------------------------------------
DROP TABLE IF EXISTS `user`;
CREATE TABLE `user` (
	`user_id`        BIGINT 	NOT NULL AUTO_INCREMENT PRIMARY KEY,    -- идентификатор пользователя в локальной системе
	`remote_user_id` BIGINT 	DEFAULT 0,                              -- идентификатор пользователя в удалённой системе
	`remote_auth_type` TINYINT	DEFAULT 0,				-- тип удалённой авторизационной системы - 0-local, 1-VK, 2-OpenID, 3-Facebook, 4-...
 	`name`			VARCHAR(128) 	NOT NULL,
 	`role`			TINYINT		NOT NULL DEFAULT 0,

	`balance`		INT 	    NOT NULL DEFAULT 0, -- баланс
	`currency_alpha`  CHAR(3)	NOT NULL,			-- буквенный код валюты из таблицы `iso3166` (устанавливается при регистрации)
	`currency_number` INT		NOT NULL,			-- цифровой код валюты из таблицы `iso3166` (устанавливается при регистрации)
	`country_number`  INT		NOT NULL,			-- цифровой код страны из таблицы `iso3166` (устанавливается при регистрации)

	`ban` 			TINYINT		DEFAULT 0,			-- флаг пользователя о бане. если > 0, то юзер заблокирован. значение - причина бана.
	`ban_comment`	VARCHAR(64) DEFAULT NULL,		-- ещё более подробное объяснение бана (устанавливается администратором)
	`password`		CHAR(64) 	NOT NULL, 	        -- md5(...), sha(...), ...
	`reg_date`		DATETIME 	DEFAULT NULL,		-- устанавливается автоматически, если создавать пользователя с помощью SP `create_user`
	`email` 		VARCHAR(128) 	DEFAULT NULL,

	`params`        VARCHAR(2048) 	DEFAULT NULL,	-- описание параметров пользователя: "[{key1, val}, {key2, val}, ...]."
	`last_login` 	DATETIME 	DEFAULT NULL,
	`last_logout` 	DATETIME 	DEFAULT NULL,
	`last_ip`		BIGINT 	 	DEFAULT 0,
	`last_ip_alpha` CHAR(32) 	DEFAULT NULL,
	`comment`		VARCHAR(64) 	DEFAULT NULL,	-- коментарий к пользователю
	`status`        TINYINT		DEFAULT 0,			-- статус пользователя (0 - не активирован, 1 - активирован)
	`partner_id`    BIGINT		DEFAULT 0			-- id партнёра, который привёл пользователя
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=1;


-- --------------------------------------------------------
-- Таблица для хранения информации о сервисах, подключенных пользователю
-- --------------------------------------------------------
DROP TABLE IF EXISTS `user_service`;
CREATE TABLE `user_service` (
    `user_service_id`   BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,-- порядковый номер подключенного сервиса
    `user_id`           BIGINT NOT NULL,            -- идентификатор пользователя
    `service_id`        INT NOT NULL,               -- идентификатор сервиса
    `node_id`           INT NOT NULL,               -- какой из узлов предоставляет сервис
    `params`            VARCHAR(2048) NOT NULL,		-- описание параметров подключенного сервиса. формат : "[{key1, val}, {key2, val}, ...]."
    `create_date`       DATETIME,                   -- дата добавления сервиса
    `end_date`          DATETIME,                   -- когда отключить сервис. NULL - без срока действия
    `status`            TINYINT NOT NULL DEFAULT 0  -- статус*
) ENGINE=InnoDB  DEFAULT CHARSET=utf8  AUTO_INCREMENT=1;

-- * 0 - не активирован, 1 - поступил в обработку, 2 - активирован, 3 - активирован+автопродление
-- * 5 - ошибка активации, пользователь забанен,
-- * 6 - ошибка активации, подключаемый сервис закрыт (недоступен для подключения)
-- * 7 - ошибка активации, нет доступных ресурсов для подключения сервиса
-- * 128 - сервис отключён

-- --------------------------------------------------------
-- Таблица для хранения информации о сервисах, которые может подключить пользователь
-- --------------------------------------------------------
DROP TABLE IF EXISTS `service`;
CREATE TABLE `service` (
    `service_id`        INT NOT NULL PRIMARY KEY,   -- идентификатор сервиса
    `name`              VARCHAR(128) NOT NULL,      -- название (напр. VPS Lite, VPS Medium, VPS Super, VPS Master \ Hosting Lite, Hosting ...)
    `type`              INT NOT NULL,               -- тип сервиса
    `params`            VARCHAR(2048) NOT NULL,     -- описание параметров сервиса. формат : "[{key1, val}, {key2, val}, {key3, val}, ...]."
    `cost`              INT NOT NULL DEFAULT 0,     -- стоимость использования по `умолчанию` (указывается в копейках)
    `period`            INT NOT NULL DEFAULT 0,     -- расчётный период в минутах. Как правило расчётный период - 1 день (1440 минут)
    `status`            TINYINT NOT NULL            -- 0 - сервис открыт, 1 - сервис закрыт
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;


-- --------------------------------------------------------
-- Таблица для хранения о ценах предоставляемых услуг
-- --------------------------------------------------------
DROP TABLE IF EXISTS `service_cost`;
CREATE TABLE `service_cost` (
    `service_id`        INT UNSIGNED NOT NULL,			-- идентификатор сервиса
	`currency_number`	INT UNSIGNED NOT NULL,			-- валюта (код по iso3166)
	`cost`              INT UNSIGNED NOT NULL DEFAULT 0, -- стоимость
	INDEX `scost_service_id_idx` (`service_id`),
	INDEX `scost_currency_number_idx` (`currency_number`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;



-- 1Tb = 1024Gb =1048576Mb
-- 2Tb = 2048Gb =2097152Mb

-- Типы сервисов :                        | Параметры (пример) :
-- 1 - OpenPanel hosting Fixed    | [{vhosts_count, 5}, {db_count, 5}, {hdd, 2048}]
-- 2 - OpenPanel hosting Scale    | [{vhost_cost, 10}, {db_cost, 10}, {hdd_cost, 10}]
-- 3 - cPanel hosting Fixed       | [{vhost_cost, 10}, {db_cost, 10}, {hdd_cost, 10}]

-- 10 - OpenNebula VPS/KVM Fixed  | [{cpus, 5}, {memory, 5}, {hdd, 10240}, {traffic, 1024000}]
-- 11 - OpenNebula VPS/KVM Scale  | 

-- 3001 - Steam CS:GO line case
-- 3002 - Steam CS:GO raffle case

-- 5000 - TCP-Proxy network filter
-- 5001 - Web-Proxy network filter

-- 2000 - Steam reveal auction

-- 3000 - Steam Market Trade CS:GO

-- 100001 - Вывод средств с партёрского счёта


-- 0x01000000 - Radio (WiFi)  internet access
-- 0x02000000 - Cable (RJ-45) internet access


-- hosting_service.params (квота)
-- Количество БД, Количество Хостов, Размер дисковой квоты


-- --------------------------------------------------------
-- Таблица описывающая группы узлов кластера
-- --------------------------------------------------------
DROP TABLE IF EXISTS `node_group`;
CREATE TABLE `node_group` (
	`group_id`			INT NOT NULL,     		-- номер группы
	`name`				VARCHAR(128) NOT NULL,	-- название группы
	`show_number`		INT NOT NULL DEFAULT 0	-- порядок отображения
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;


-- --------------------------------------------------------
-- Таблица описывающая узлы кластера, обслуживающих различные сервисы
-- --------------------------------------------------------
DROP TABLE IF EXISTS `node`;
CREATE TABLE `node` (
	`node_id`           INT NOT NULL,               -- номер узла
	`hostname`          VARCHAR(128) NOT NULL,      -- имя хоста для данного узла
	`ip`                VARCHAR(64) NOT NULL,       -- ip адрес узла
	`service_type` 		INT NOT NULL,               -- какой тип сервиса обслуживает узел
	`comment`			VARCHAR(128) DEFAULT NULL,	-- комментарий у узлу
	`group_id`			INT NOT NULL DEFAULT 0,		-- номер группы
	`show_number`		INT NOT NULL DEFAULT 0,		-- номер отображения
	`icon`				VARCHAR(128) DEFAULT NULL,	-- ссылка на картинку
	`status`            TINYINT NOT NULL            -- 0 - узел открыт, n=(1, 2, 3, 4) - узел закрыт
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;


-- --------------------------------------------------------
-- Таблица для хранения информации о заказах
-- --------------------------------------------------------
DROP TABLE IF EXISTS `order`;
CREATE TABLE `order` (
    `order_id`    INT NOT NULL AUTO_INCREMENT PRIMARY KEY,  -- номер заказа
    `user_id`	  BIGINT NOT NULL,		    	        -- идентификатор пользователя - `чей заказ`
    `service_id`  INT NOT NULL,                     -- идентификатор подключаемого сервиса
    `params`      VARCHAR(512) NOT NULL,
    `period`      INT NOT NULL DEFAULT 0,           -- оплачиваемый период действия (в минутах)
    `status`	  TINYINT NOT NULL DEFAULT 0,       -- статус заказа *
    `processing_mode` TINYINT NOT NULL DEFAULT 0,   -- режим обработки (0 - автоматически, 1 - по факту оплаты, 2 - ручник)
    `create_date` DATETIME,
    `close_date`  DATETIME
) ENGINE=InnoDB  DEFAULT CHARSET=utf8  AUTO_INCREMENT=1;

-- * статусы заказа :
-- 0 - не обработан, 1 - поступил в обработку, 2 - обработан
-- 3 - обработан, но денег для подключения сервиса нет



-- Транзакция - в общем случае, любая операция с использованием личного счёта. 

-- --------------------------------------------------------
-- Таблица для хранения информации о транзакциях биллинг системы
-- --------------------------------------------------------
DROP TABLE IF EXISTS `transaction`;
CREATE TABLE `transaction` (
	`transaction_id`	BIGINT 		NOT NULL AUTO_INCREMENT PRIMARY KEY,
	`type`				INT			NOT NULL, -- тип. [0 - оплата включенного сервиса, 1 - оплата заказа, 10 - пополнение ЛС, 1000 - изменение партнёрского баланса]
	`params`      		VARCHAR(2048) 	NOT NULL, -- параметры транзакции. формат : "[{key1, val}, {key2, val}, {key3, val}, ...]"
	`user_id`			BIGINT 		NOT NULL,

	`currency_alpha`  	CHAR(5)	NOT NULL,			-- буквенный код валюты
	`currency_number` 	INT		NOT NULL,			-- цифровой код валюты

	`cost`				INT 		NOT NULL DEFAULT 0, -- сумма транзакции (сколько снимаем или пополняем)
	`new_balance` 		INT 		NOT NULL DEFAULT 0,	-- баланс ПОСЛЕ выполнения транзакции
	`status`			TINYINT		NOT NULL DEFAULT 0,	-- статус транзакции. 0 - создана. 1 = успешно закрыта. 2,3,4,5... = коды ошибок
	`create_date` 		DATETIME	DEFAULT NULL,
	`close_date` 		DATETIME	DEFAULT NULL
) ENGINE=InnoDB  DEFAULT CHARSET=utf8  AUTO_INCREMENT=1;


-- --------------------------------------------------------
-- Таблица для хранения информации о партнёрах
-- --------------------------------------------------------
DROP TABLE IF EXISTS `partner`;
CREATE TABLE `partner` (
	`partner_id`	BIGINT		NOT NULL AUTO_INCREMENT PRIMARY KEY,	-- id партнёра
	`user_id`		BIGINT		DEFAULT NULL,					-- id пользователя партнёра
	`type`			TINYINT		NOT NULL DEFAULT 0,				-- тип партнёрской программы
	`key`			VARCHAR(32) DEFAULT NULL,					-- уникальный ключ партнёра	
	`balance`		INT 	    NOT NULL DEFAULT 0				-- партнёрский баланс
) ENGINE=InnoDB  DEFAULT CHARSET=utf8  AUTO_INCREMENT=1;




-- --------------------------------------------------------
-- Таблица описаний записей в журнале
-- --------------------------------------------------------
DROP TABLE IF EXISTS `log_desc`;
CREATE TABLE `log_desc` (
    `log_desc_id`	BIGINT UNSIGNED 	NOT NULL PRIMARY KEY,
    `template_ru`	VARCHAR(512)		NOT NULL,
    `template_en`	VARCHAR(512)		NOT NULL,
    `access_level`	TINYINT				NOT NULL 				-- уровень доступа
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;


-- --------------------------------------------------------
-- Таблица для записей журнала
-- --------------------------------------------------------
DROP TABLE IF EXISTS `log`;
CREATE TABLE `log` (
	`log_id`              BIGINT UNSIGNED  NOT NULL AUTO_INCREMENT PRIMARY KEY,    -- идентификатор записи в журнале
	`log_desc_id`	      BIGINT UNSIGNED	NOT NULL,				-- идентификатор действия и формата записи (шаблона)
	`log_template_params` VARCHAR(512)	NOT NULL,					-- параметры для подстановки в шаблон
	`user_id`             BIGINT            NOT NULL,				-- идентификатор субъекта системы, произведшего действие
	`create_date`         DATETIME          DEFAULT NULL,			-- устанавливается автоматически
	`subject_ip`          VARBINARY(16)     DEFAULT NULL			-- ip-адрес субъекта, с которого совершено действие
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=1;


-- --------------------------------------------------------
-- Таблица для записей о промокодах
-- --------------------------------------------------------
DROP TABLE IF EXISTS `promocode`;
CREATE TABLE `promocode` (
	`promocode_id`		BIGINT UNSIGNED		NOT NULL AUTO_INCREMENT PRIMARY KEY,    -- идентификатор
    `type`				TINYINT 			NOT NULL,				-- тип промокода *
    `key`				CHAR(16)			NOT NULL,				-- ключ промокода
	`params`			VARCHAR(2048)		NOT NULL,				-- описание параметров промокода (на всякий случай). формат : "[{key1, val}, ...]"
    `end_date`			DATETIME			DEFAULT NULL,			-- время окончания работы промокода
	`activate_cur`		INT UNSIGNED 		NOT NULL,				-- текущее количество активаций
	`activate_max`		INT UNSIGNED 		NOT NULL,				-- максимальное количество активаций
    `status`            TINYINT 			NOT NULL,				-- 0 - промокод открыт, 1 - промокод закрыт
	INDEX `promocode_type_idx` (`type`),
	INDEX `promocode_key_idx` (`key`),
	INDEX `promocode_status_idx` (`status`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=1;


-- --------------------------------------------------------
-- Таблица для записей об активациях промокодов
-- --------------------------------------------------------
DROP TABLE IF EXISTS `user_promocode`;
CREATE TABLE `user_promocode` (
    `user_promocode_id`	BIGINT UNSIGNED 	NOT NULL AUTO_INCREMENT PRIMARY KEY,	-- уникальный идентификатор активации
	`promocode_id`		BIGINT UNSIGNED		NOT NULL,    			-- идентификатор описателя промокода
	`user_id`			BIGINT UNSIGNED		NOT NULL,				-- id пользователя
    `params`            VARCHAR(2048)		NOT NULL,				-- описание параметров промокода (на всякий случай). формат : "[{key1, val}, ...]"
    `activate_date`		DATETIME			NOT NULL,				-- дата активации промокода
	`activate_ip`		BIGINT 	 			DEFAULT 0,				-- IP адрес с которого производилась активация (цифровой вид)
	`activate_ip_alpha`	CHAR(32) 	 		DEFAULT NULL,			-- IP адрес с которого производилась активация (цифровой вид)
    `status`            TINYINT 			NOT NULL,				-- 0 - промокод не активирован, 1 - промокод активирован
	INDEX `up_promocode_id_idx` (`promocode_id`),
	INDEX `up_user_id_idx` (`user_id`),
	INDEX `up_activate_date_idx` (`activate_date`),
	INDEX `up_status_idx` (`status`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 AUTO_INCREMENT=1;


-- --------------------------------------------------------
-- Таблица для хранения информации о странах и их кодах валют (iso3166 + iso4217)
-- --------------------------------------------------------
DROP TABLE IF EXISTS `iso3166`;
CREATE TABLE `iso3166` (
    `country_number`	INT UNSIGNED NOT NULL PRIMARY KEY, -- 3 символьный цифровой код страны
	`country_name`		VARCHAR(64),		-- название страны
	`country_locale`	VARCHAR(3),			-- локаль страны
    `country_alpha_2`	CHAR(2) NOT NULL,	-- 2 символьный буквенный код страны
    `country_alpha_3`	CHAR(3) NOT NULL,	-- 3 символьный буквенный код страны
    `currency_alpha_3`	CHAR(3) NOT NULL,	-- 3 символьный буквенный код валюты страны (по iso4217)
    `currency_number_3`	INT NOT NULL,		-- 3 символьный цифровой код валюты (по iso4217)
	INDEX `iso3166_country_number_idx` (`country_number`),
	INDEX `iso3166_country_alpha_2_idx` (`country_alpha_2`),
	INDEX `iso3166_currency_number_3_idx` (`currency_number_3`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;



-- --------------------------------------------------------
-- Таблица для хранения информации базы ip адресов (v4) и стран (ip2location)
-- --------------------------------------------------------
DROP TABLE IF EXISTS `ip2location_v4_db1`;
CREATE TABLE `ip2location_v4_db1`(
	`ip_from`		INT(10) UNSIGNED,
	`ip_to`			INT(10) UNSIGNED,
	`country_code`	CHAR(2),		-- `iso3166.country_alpha_2`
	`country_name`	VARCHAR(64), 	-- `iso3166.country_name`
	INDEX `ip2l_db1_ip_from_idx` (`ip_from`),
	INDEX `ip2l_db1_ip_to_idx` (`ip_to`),
	INDEX `ip2l_db1_ip_from_to_idx` (`ip_from`, `ip_to`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;



-- Создаём индексы
CREATE INDEX IF NOT EXISTS `tr_type` ON transaction (type);
CREATE INDEX IF NOT EXISTS `tr_user_id_idx` ON transaction (user_id);
CREATE INDEX IF NOT EXISTS `tr_status_idx` ON transaction (status);
CREATE INDEX IF NOT EXISTS `tr_cdate_idx` ON transaction (create_date);
CREATE INDEX IF NOT EXISTS `tr_ccy_number_idx` ON transaction (currency_number);


CREATE INDEX IF NOT EXISTS `user_rid_idx` ON `user` (`remote_user_id`);
CREATE INDEX IF NOT EXISTS `user_partner_id_idx` ON `user` (`partner_id`);


CREATE INDEX IF NOT EXISTS `user_service_sidx` ON `user_service` (`service_id`);
CREATE INDEX IF NOT EXISTS `user_service_statusidx` ON `user_service` (`status`);
CREATE INDEX IF NOT EXISTS `user_service_paramsidx` ON `user_service` (`params`);
CREATE INDEX IF NOT EXISTS `user_service_cdateidx` ON `user_service` (`create_date`);
CREATE INDEX IF NOT EXISTS `service_typeidx` ON `service` (`type`);

-- ######################## DEBUG INSERTS ######################## 

-- LOCK TABLES `user` WRITE;
-- INSERT INTO `user` (`remote_user_id`, `name`, `password`, `email`) VALUES (0, 'admin', 'F4rfg6s', '');
-- INSERT INTO `user` (`remote_user_id`, `name`, `password`, `email`) VALUES (0, 'admin', 'V9c8cgffdkg4', '');
-- INSERT INTO `user` (`remote_user_id`, `name`, `password`, `email`) VALUES (0, 'kraysx', '8cddfg%gSLbxd', '');
-- UNLOCK TABLES;


-- ALTER TABLE `transaction` MODIFY `params` VARCHAR(2048) NOT NULL;
-- ALTER TABLE `user_promocode` ADD `activate_ip` BIGINT DEFAULT 0 AFTER `activate_date`;
-- ALTER TABLE `user_promocode` ADD `activate_ip_alpha`	CHAR(32)	DEFAULT NULL AFTER `activate_ip`;
-- ALTER TABLE `user_promocode` DROP COLUMN `activate_ip`;


