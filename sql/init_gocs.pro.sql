USE `cloudbilling`;

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
-- Таблица для хранения информации о сервисах, которые может подключить пользователь
-- --------------------------------------------------------
DROP TABLE IF EXISTS `service`;
CREATE TABLE `service` (
    `service_id`        INT NOT NULL PRIMARY KEY,   -- идентификатор сервиса
    `name`              VARCHAR(128) NOT NULL,      -- название (напр. VPS Lite, VPS Medium, VPS Super, VPS Master \ Hosting Lite, Hosting ...)
    `type`              INT NOT NULL,               -- тип сервиса
    `params`            VARCHAR(2048) NOT NULL,     -- описание параметров сервиса. формат : "[{key1, val}, {key2, val}, {key3, val}, ...]"
    `cost`              INT NOT NULL DEFAULT 0,     -- стоимость использования за один расчётный период (указывается в копейках)
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


-- --------------------------------------------------------
-- Таблица описаний записей в журнале
-- --------------------------------------------------------
DROP TABLE IF EXISTS `log_desc`;
CREATE TABLE `log_desc` (
    `log_desc_id`	BIGINT UNSIGNED 	NOT NULL PRIMARY KEY,
    `template_ru`	VARCHAR(512)		NOT NULL,
    `template_en`	VARCHAR(512)		NOT NULL,
    `access_level`      TINYINT                 NOT NULL                -- уровень доступа
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;

-- --------------------------------------------------------
-- Таблица для записей журнала
-- --------------------------------------------------------
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

-- * тип промокода (логика обработки типов зашита прямо в контроллер steamraffle_promocode_controller.erl)
-- 1-начисление 100RP. пользователь может активировать промокод 1 раз в сутки
-- 2-открытие кейса за RP очки
-- халявная армейка

-- --------------------------------------------------------
-- Таблица для записей об активациях промокодов
-- --------------------------------------------------------
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


-- Таблица узлов-обработчиков (боты и др.)
LOCK TABLES `node` WRITE;
INSERT INTO `node` VALUES 
--	(1,  'bot1.steam.gocs.pro', '127.0.0.1', 91000, '', 0, 0, '', 0), -- dev Reveal Auction   91000
--	(2,  'bot2.steam.gocs.pro', '127.0.0.1', 3001, '', 0, 0, '', 0),
--	(3,  'bot3.steam.gocs.pro', '127.0.0.1', 3002, '', 0, 0, '', 0),
--	(4,  'bot4.steam.gocs.pro', '127.0.0.1', 3050, '', 0, 0, '', 0),
--	(5,  'bot5.steam.gocs.pro', '127.0.0.1', 3001, '', 0, 0, '', 0),
--	(6,  'bot6.steam.gocs.pro', '127.0.0.1', 3002, '', 0, 0, '', 0),
--	(7,  'bot7.steam.gocs.pro', '127.0.0.1', 3002, '', 0, 0, '', 0),
--	(8,  'node8.steamcloud.progame.ru', '127.0.0.1', 3001, '', 0, 0, '', 0),
	(9,  'node9.steamcloud.progame.ru', '127.0.0.1', 91000, '', 0, 0, '', 0),
	(10, 'node10.steamcloud.progame.ru', '127.0.0.1', 3060, '', 0, 0, '', 0), -- gocs.pro Crystal Case
	(11, 'node11.steamcloud.progame.ru', '127.0.0.1', 3080, '', 0, 0, '', 0), -- gocs.pro Quantum Case

	(12, 'node12.steamcloud.progame.ru', '127.0.0.1', 3120, '', 0, 0, '', 0), -- gocs.pro PROMO case

	(13, 'node13.steamcloud.progame.ru', '127.0.0.1', 3100, '', 0, 0, '', 0), -- gocs.pro Rank Silver Case

	(14, 'node14.steamcloud.progame.ru', '127.0.0.1', 3101, '', 0, 0, '', 0), -- gocs.pro Rank Gold Case   /steamlottery.ru тайный/засекреченый
	(15, 'node15.steamcloud.progame.ru', '127.0.0.1', 3102, '', 0, 0, '', 0), -- gocs.pro Rank Platinum Case   /steamlottery.ru обычные кейсы

	(16, 'node16.steamcloud.progame.ru', '127.0.0.1', 3120, '', 0, 0, '', 0), -- gocs.pro PROMO case

	(17, 'node17.steamcloud.progame.ru', '127.0.0.1', 5000, '', 0, 0, '', 0), -- хранилище на автозакуп
	(18, 'node18.steamcloud.progame.ru', '127.0.0.1', 5000, '', 0, 0, '', 0), -- хранилище на автозакуп

	(19, 'node19.steamcloud.progame.ru', '127.0.0.1', 3001, '', 0, 0, '', 0), -- gocs.pro обычные кейсы
	(20, 'node20.steamcloud.progame.ru', '127.0.0.1', 3001, '', 0, 0, '', 0), -- gocs.pro Glove,Gamma2 (обычные кейсы без замен)
--	(21, 'node21.steamcloud.progame.ru', '127.0.0.1', 3001, '', 0, 0, '', 0), -- gocs.pro резерв (вообще виртуалка не настроена)
	(22, 'node22.steamcloud.progame.ru', '127.0.0.1', 3001, '', 0, 0, '', 0), -- auction.gocs.pro (Reveal Auction)
	(23, 'node23.steamcloud.progame.ru', '127.0.0.1', 3001, '', 0, 0, '', 0), -- gocs.pro обычные кейсы
	(24, 'node24.steamcloud.progame.ru', '127.0.0.1', 3070, '', 0, 0, '', 0), -- gocs.pro кейсы по типу (m4a1-s, m4a4, awp, ak47, deagle)
	(25, 'node25.steamcloud.progame.ru', '127.0.0.1', 3002, '', 0, 0, '', 0), -- gocs.pro Consumer Raffle
	(26, 'node26.steamcloud.progame.ru', '127.0.0.1', 3002, '', 0, 0, '', 0), -- gocs.pro Consumer Raffle
	(27, 'node27.steamcloud.progame.ru', '127.0.0.1', 3002, '', 0, 0, '', 0), -- gocs.pro Classified Raffle + Covert Raffle
	(28, 'node28.steamcloud.progame.ru', '127.0.0.1', 3003, '', 0, 0, '', 0), -- gocs.pro Knife Raffle
	(29, 'node29.steamcloud.progame.ru', '127.0.0.1', 3040, '', 0, 0, '', 0), -- gocs.pro Stat Trak Raffle
	(30, 'node30.steamcloud.progame.ru', '127.0.0.1', 3050, '', 0, 0, '', 0); -- gocs.pro Dragon Lore Raffle

--	(113, 'node13.steamcloud.progame.ru', '127.0.0.1', 3120, '', 0, 0, '', 0); -- gocs.pro Promo Case
UNLOCK TABLES;


-- 3001 - Обычный кейс-рандом CS:GO с привязкой номер игры->качество предмета для каждого пользователя
-- 3002 - Спец. Кейс рандом CS:GO случайного предмета заданного качества


-- 9002 - Список предметов с описанием..вообще уже не нужен


-- Таблица сервисов для работы с финансами
LOCK TABLES `service` WRITE;
INSERT INTO `service` VALUES
    (100001, 'Partner withdraw', 100001, '[].', 0, 0, 0);
UNLOCK TABLES;




-- LOCK TABLES `service` WRITE;
-- INSERT INTO `service` VALUES
-- UNLOCK TABLES;



-- Таблица сервисов - кейсов, доступных для открытия
LOCK TABLES `service` WRITE;
INSERT INTO `service` VALUES
    (10, 'Chroma Case'    			, 3001, '[ {case_type, normalpromo}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_6"}]} ].', 9900, 0, 0), -- 89 рублей (9900 копеек)
    (11, 'Phoenix Case'   			, 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_2"}]} ].', 9900, 0, 0), -- 89 рублей
    (12, 'Breakout Case'			, 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_4"}]} ].', 9900, 0, 0), -- 99 рублей
    (13, 'Vanguard Case'			, 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_5"}]} ].', 9900, 0, 0),
    (14, 'Operation Bravo'			, 3001, '[ {case_type, normal}, {play_line_key, operation_bravo}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_bravo_i"}]} ].'   , 14900, 0, 0),
    (15, 'Chroma 2 Case'			, 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_7"}]} ].', 9900, 0, 0),
    (16, 'Winter Offensive'			, 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_1"}]} ].', 9900, 0, 0),
    (17, 'Huntsman Case'			, 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_3"}]} ].', 9900, 0, 0),
    (18, 'Оружейный кейс: Тираж #2' , 3001, '[].', 9900, 0, 0),
    (19, 'Оружейный кейс: Тираж #3' , 3001, '[].', 9900, 0, 0),
    (20, 'eSports 2013'             , 3001, '[ {case_type, normal}, {play_line_key, esports_2013}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_esports"}]} ].', 14900, 0, 0),
    (21, 'eSports 2013 Winter'      , 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_esports_ii"}]} ].', 9900, 0, 0),
    (22, 'eSports 2014 Summer'		, 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_esports_iii"}]} ].', 9900, 0, 0),

    (23, 'Falchion Case'			, 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_8"}]} ].', 9900, 0, 0),

	(24, 'Wildfire Case'			, 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_11"}]} ].', 9900, 0, 0),
	(25, 'Revolver Case'			, 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_10"}]} ].', 9900, 0, 0),
	(26, 'Shadow Case'				, 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_9"}]} ].', 9900, 0, 0),
	(27, 'Chroma 3 Case'			, 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_12"}]} ].', 9900, 0, 0),
	(28, 'Gamma Case'				, 3001, '[ {case_type, normal}, {play_line_key, base_play_line}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_13"}]} ].', 9900, 0, 0),
	(1000, 'Gamma 2 Case'			, 3001, '[ {case_type, normal}, {play_line_key, gamma_case}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_gamma_2"}]} ].', 14900, 0, 0),
	(1001, 'Glove Case'				, 3001, '[ {case_type, normal}, {play_line_key, gamma_case}, {rank_points, 30}, {tag_filter, [#{category => "ItemSet", internal_name => "set_community_15"}]} ].', 14900, 0, 0),

    (29, 'Consumer Raffle'        	, 3002, '[ {case_type, normal}, {play_line_key, consumer_raffle}, {rank_points, 5}, {tag_filter, [#{category => "Rarity", internal_name => "Rarity_Common_Weapon"}]} ].', 1000, 0, 0),
    (30, 'Classified Raffle'        , 3002, '[ {case_type, normal}, {play_line_key, classified_raffle}, {rank_points, 70}, {tag_filter, [#{category => "Rarity", internal_name => "Rarity_Legendary_Weapon"}]} ].', 44900, 0, 0),
    (31, 'Covert Raffle'        	, 3002, '[ {case_type, normal}, {play_line_key, covert_raffle}, {rank_points, 70}, {tag_filter, [#{category => "Rarity", internal_name => "Rarity_Ancient_Weapon"}]} ].', 59900, 0, 0),
    (32, 'Knife Raffle'             , 3003, '[ {case_type, normal}, {play_line_key, knife_raffle}, {rank_points, 500}, {tag_filter, [#{category => "Type", internal_name => "CSGO_Type_Knife"}]} ].', 349900, 0, 0),

    (40, 'Stat Track Raffle'        , 3040, '[ {case_type, normal}, {play_line_key, stat_trak_raffle}, {rank_points, 100}, {tag_filter, []} ].', 79900, 0, 0),
    (50, 'Dragon Lore Raffle'       , 3050, '[ {case_type, normal}, {play_line_key, dragon_lore_raffle}, {rank_points, 1000}, {tag_filter, []} ].', 599900, 0, 0),
    (60, 'Crystal Case'       		, 3060, '[ {case_type, normal}, {play_line_key, crystal_case}, {rank_points, 70}, {tag_filter, []} ].', 9900, 0, 0),
	(61, 'Twitch Case'       		, 3061, '[ {case_type, promo}, {play_line_key, twitch_case}, {rank_points, 0}, {tag_filter, []} ].', 0, 0, 0),
--    (62, 'Crystal Case'       		, 3060, '[ {case_type, normal}, {play_line_key, crystal2_case}, {rank_points, 70}, {tag_filter, []} ].', 9900, 0, 0),

	(70, 'M4A1-S'					, 3070, '[ {case_type, normal}, {play_line_key, m4a1_s_raffle}, {rank_points, 50}, {tag_filter, [#{category => "Weapon", internal_name => "weapon_m4a1_silencer"}]} ].', 28900, 0, 0), -- m4a1_s_raffle
	(71, 'M4A4'						, 3070, '[ {case_type, normal}, {play_line_key, m4a1_s_raffle}, {rank_points, 50}, {tag_filter, [#{category => "Weapon", internal_name => "weapon_m4a1"}]} ].', 28900, 0, 0), -- m4a4_raffle
	(72, 'AK-47'					, 3070, '[ {case_type, normal}, {play_line_key, m4a1_s_raffle}, {rank_points, 50}, {tag_filter, [#{category => "Weapon", internal_name => "weapon_ak47"}]} ].', 28900, 0, 0), -- ak_47_raffle
	(73, 'AWP'						, 3070, '[ {case_type, normal}, {play_line_key, m4a1_s_raffle}, {rank_points, 50}, {tag_filter, [#{category => "Weapon", internal_name => "weapon_awp"}]} ].', 28900, 0, 0), -- awp_raffle
	(74, 'Desert Eagle'				, 3070, '[ {case_type, normal}, {play_line_key, deagle_raffle}, {rank_points, 50}, {tag_filter, [#{category => "Weapon", internal_name => "weapon_deagle"}]} ].', 12900, 0, 0),

--	(80, 'Quantum Case'				, 3080, '[ {case_type, normal}, {play_line_key, quantum_case}, {rank_points, 50}, {tag_filter, []} ].', 30000, 0, 0),
--	(81, 'fEss Case'				, 3080, '[ {case_type, normal}, {play_line_key, quantum_case}, {rank_points, 50}, {tag_filter, []} ].', 30000, 0, 0),
--	(82, 'Dimonsterus Case'			, 3080, '[ {case_type, normal}, {play_line_key, quantum_case}, {rank_points, 50}, {tag_filter, []} ].', 30000, 0, 0),
--	(83, 'Nyaruko Game Case'		, 3080, '[ {case_type, normal}, {play_line_key, quantum_case}, {rank_points, 50}, {tag_filter, []} ].', 30000, 0, 0),
--	(84, 'Mr.Obzorka Case'			, 3080, '[ {case_type, normal}, {play_line_key, quantum_case}, {rank_points, 50}, {tag_filter, []} ].', 30000, 0, 0),

	(100, 'Rank Silver Case'		, 3100, '[ {case_type, rank}, {play_line_key, rank_silver_case},   {rank_points_cost, 100},  {min_added_funds, #{<<"RUB">>=>10000, <<"EUR">>=>200,<<"USD">>=>200}}, {tag_filter, []} ].', 0, 0, 0),
	(101, 'Rank Gold Case'			, 3101, '[ {case_type, rank}, {play_line_key, rank_gold_case},     {rank_points_cost, 1500}, {min_added_funds, #{<<"RUB">>=>50000, <<"EUR">>=>1000,<<"USD">>=>1000}}, {tag_filter, []} ].', 0, 0, 0),
	(102, 'Rank Platinum Case'		, 3102, '[ {case_type, rank}, {play_line_key, rank_platinum_case}, {rank_points_cost, 5000}, {min_added_funds, #{<<"RUB">>=>200000,<<"EUR">>=>4000,<<"USD">>=>4000}}, {tag_filter, []} ].', 0, 0, 0),

	(120, 'Promo Case'				, 3120, '[ {case_type, promo}, {play_line_key, promo_case}, {rank_points, 0}, {tag_filter, []} ].', 0, 0, 0),
	(121, 'Shurigina Case'			, 3080, '[ {case_type, normal}, {play_line_key, quantum_case}, {rank_points, 10}, {tag_filter, []} ].', 10000, 0, 0),

	(200, 'The Cobblestone Collection'	, 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_cobblestone"}]} ].', 0, 0, 0),
	(201, 'The Gods And Monsters Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_gods_and_monsters"}]} ].', 0, 0, 0),
	(202, 'The Chop Shop Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_chopshop"}]} ].', 0, 0, 0),
	(203, 'The Overpass Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_overpass"}]} ].', 0, 0, 0),
	(204, 'The Rising Sun Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_kimono"}]} ].', 0, 0, 0),
	(205, 'The Assault Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_assault"}]} ].', 0, 0, 0),
	(206, 'The Baggage Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_baggage"}]} ].', 0, 0, 0),
	(207, 'The Cache Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_cache"}]} ].', 0, 0, 0),
	(208, 'The Safehouse Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_safehouse"}]} ].', 0, 0, 0),
	(209, 'The Militia Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_militia"}]} ].', 0, 0, 0),
	(210, 'The Dust 2 Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_dust_2"}]} ].', 0, 0, 0),
	(211, 'The Bank Collection'	, 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_bank"}]} ].', 0, 0, 0),
	(212, 'The Mirage Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_mirage"}]} ].', 0, 0, 0),
	(213, 'The Nuke Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_nuke"}]} ].', 0, 0, 0),
	(214, 'The Dust Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_dust"}]} ].', 0, 0, 0),
	(215, 'The Office Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_office"}]} ].', 0, 0, 0),
	(216, 'The Italy Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_italy"}]} ].', 0, 0, 0),
	(217, 'The Inferno Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_inferno"}]} ].', 0, 0, 0),
	(218, 'The Vertigo Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_vertigo"}]} ].', 0, 0, 0),
	(219, 'The Aztec Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_aztec"}]} ].', 0, 0, 0),
	(220, 'The Train Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_train"}]} ].', 0, 0, 0),
	(221, 'The Lake Collection'	, 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_lake"}]} ].', 0, 0, 0),
	(222, 'The Alpha Collection', 3001, '[ {case_type, normal}, {play_line_key, coll_base_line}, {rank_points, 0}, {tag_filter, [#{category => "ItemSet", internal_name => "set_bravo_ii"}]} ].', 0, 0, 0),

    (10001, 'Mythical Case'			, 57001, '[ {case_type, normal}, {play_line_key, dota2_mythical_play_line}, {tag_filter, [#{category => "Rarity", internal_name => "Rarity_Mythical"}]} ].', 9900, 0, 0),
    (10002, 'Legendary Case'		, 57001, '[ {case_type, normal}, {play_line_key, dota2_legendary_play_line}, {tag_filter, [#{category => "Rarity", internal_name => "Rarity_Legendary"}]} ].', 19900, 0, 0),
    (10003, 'Immortal Case'			, 57001, '[ {case_type, normal}, {play_line_key, dota2_immortal_play_line}, {tag_filter, [#{category => "Rarity", internal_name => "Rarity_Immortal"}]} ].', 29900, 0, 0),
    (10004, 'Arcana Case'			, 57002, '[ {case_type, normal}, {play_line_key, dota2_arkana_play_line}, {tag_filter, []} ].', 79900, 0, 0),

    (2000, 'Reveal Auction CS:GO'	, 91000, '[ ].', 0, 0, 0); -- Аукцион на понижение с закрытой ценой, лоты хранятся в user_service
UNLOCK TABLES;


-- Таблица цен сервисов - кейсов в разных валютах
LOCK TABLES `service_cost` WRITE;
INSERT INTO `service_cost` VALUES
	-- (RUB, 643)
    (10, 643, 9900),-- 'Chroma Case'
    (11, 643, 9900),-- 'Phoenix Case'
    (12, 643, 9900),-- 'Breakout Case'
    (13, 643, 9900),-- 'Vanguard Case'
    (14, 643, 14900),-- 'Operation Bravo'
    (15, 643, 9900),-- 'Chroma 2 Case'
    (16, 643, 9900),-- 'Winter Offensive'
    (17, 643, 9900),-- 'Huntsman Case'
    (18, 643, 9900),-- 'Оружейный кейс: Тираж #2'
    (19, 643, 9900),-- 'Оружейный кейс: Тираж #3'
    (20, 643, 14900),-- 'eSports 2013'
    (21, 643, 9900),-- 'eSports 2013 Winter'
    (22, 643, 9900),-- 'eSports 2014 Summer'

    (23, 643, 9900),-- 'Falchion Case'

	(24, 643, 9900),-- 'Wildfire Case'
	(25, 643, 9900),-- 'Revolver Case'
	(26, 643, 9900),-- 'Shadow Case'
	(27, 643, 9900),-- 'Chroma 3 Case'
	(28, 643, 9900),-- 'Gamma Case'
	(1000, 643, 14900),-- 'Gamma 2 Case'
	(1001, 643, 14900),-- 'Glove Case'

    (29, 643, 1000),-- 'Consumer Raffle'
    (30, 643, 44900),-- 'Classified Raffle'
    (31, 643, 59900),-- 'Covert Raffle'
    (32, 643, 349900),-- 'Knife Raffle'

    (40, 643, 79900),-- 'Stat Track Raffle'
    (50, 643, 599900),-- 'Dragon Lore Raffle'
    (60, 643, 9900),-- 'Crystal Case'
    (61, 643, 0),-- 'Twitch Case'
    (62, 643, 9900),-- 'Crystal2 Case'

	(70, 643, 28900),-- 'M4A1-S'
	(71, 643, 28900),-- 'M4A4'
	(72, 643, 28900),-- 'AK-47'
	(73, 643, 28900),-- 'AWP'
	(74, 643, 12900),-- 'Desert Eagle'

--	(80, 643, 30000),-- 'Quantum Case'
--	(81, 643, 30000),-- 'fEss Case'
--	(82, 643, 30000),-- 'Dimonsterus Case'
--	(83, 643, 30000),-- 'Nyaruko Game Case'
--	(84, 643, 30000),-- 'Mr.Obzorka Case'

	(100, 643, 0),-- 'Rank Silver Case'
	(101, 643, 0),-- 'Rank Gold Case'
	(102, 643, 0),-- 'Rank Platinum Case'

	(120, 643, 0),-- 'Promo Case'
	(121, 643, 10000),-- 'Shurigina Case'

	(200, 643, 11600),-- 'The Cobblestone Collection'
	(201, 643, 12500),-- 'The Gods And Monsters Collection'
	(202, 643, 8700),-- 'The Chop Shop Collection'
	(203, 643, 8700),-- 'The Overpass Collection'
	(204, 643, 8300),-- 'The Rising Sun Collection'
	(205, 643, 16300),-- 'The Assault Collection'
	(206, 643, 2900),-- 'The Baggage Collection'
	(207, 643, 4100),-- 'The Cache Collection'
	(208, 643, 2900),-- 'The Safehouse Collection'
	(209, 643, 8300),-- 'The Militia Collection'
	(210, 643, 1700),-- 'The Dust 2 Collection'
	(211, 643, 1100),-- 'The Bank Collection'
	(212, 643, 8300),-- 'The Mirage Collection'
	(213, 643, 8300),-- 'The Nuke Collection'
	(214, 643, 8700),-- 'The Dust Collection'
	(215, 643, 12500),-- 'The Office Collection'
	(216, 643, 1700),-- 'The Italy Collection'
	(217, 643, 4100),-- 'The Inferno Collection'
	(218, 643, 8300),-- 'The Vertigo Collection'
	(219, 643, 2900),-- 'The Aztec Collection'
	(220, 643, 1500),-- 'The Train Collection'
	(221, 643, 1500),-- 'The Lake Collection'
	(222, 643, 8700),-- 'The Alpha Collection'

	-- (EUR, 978)
    (10, 978, 180),-- 'Chroma Case'
    (11, 978, 180),-- 'Phoenix Case'
    (12, 978, 180),-- 'Breakout Case'
    (13, 978, 180),-- 'Vanguard Case'
    (14, 978, 242),-- 'Operation Bravo'
    (15, 978, 180),-- 'Chroma 2 Case'
    (16, 978, 180),-- 'Winter Offensive'
    (17, 978, 180),-- 'Huntsman Case'
    (18, 978, 180),-- 'Оружейный кейс: Тираж #2'
    (19, 978, 180),-- 'Оружейный кейс: Тираж #3'
    (20, 978, 242),-- 'eSports 2013'
    (21, 978, 180),-- 'eSports 2013 Winter'
    (22, 978, 180),-- 'eSports 2014 Summer'

    (23, 978, 180),-- 'Falchion Case'

	(24, 978, 180),-- 'Wildfire Case'
	(25, 978, 180),-- 'Revolver Case'
	(26, 978, 180),-- 'Shadow Case'
	(27, 978, 180),-- 'Chroma 3 Case'
	(28, 978, 180),-- 'Gamma Case'
	(1000, 978, 242),-- 'Gamma 2 Case'
	(1001, 978, 242),-- 'Glove Case'

    (29, 978, 27),-- 'Consumer Raffle'
    (30, 978, 630),-- 'Classified Raffle'
    (31, 978, 920),-- 'Covert Raffle'
    (32, 978, 7000),-- 'Knife Raffle'

    (40, 978, 1150),-- 'Stat Track Raffle'
    (50, 978, 11500),-- 'Dragon Lore Raffle'
    (60, 978, 161),-- 'Crystal Case'
    (61, 978, 0),-- 'Twitch Case'
    (62, 978, 161),-- 'Crystal2 Case'

	(70, 978, 460),-- 'M4A1-S'
	(71, 978, 460),-- 'M4A4'
	(72, 978, 460),-- 'AK-47'
	(73, 978, 460),-- 'AWP'
	(74, 978, 210),-- 'Desert Eagle'

--	(80, 978, 400),-- 'Quantum Case'
--	(81, 978, 400),-- 'fEss Case'
--	(82, 978, 400),-- 'Dimonsterus Case'
--	(83, 978, 400),-- 'Nyaruko Game Case'
--	(84, 978, 400),-- 'Mr.Obzorka Case'

	(100, 978, 0),-- 'Rank Silver Case'
	(101, 978, 0),-- 'Rank Gold Case'
	(102, 978, 0),-- 'Rank Platinum Case'

	(120, 978, 0),-- 'Promo Case'
	(121, 978, 200),-- 'Shurigina Case'

	(200, 978, 189),-- 'The Cobblestone Collection'
	(201, 978, 204),-- 'The Gods And Monsters Collection'
	(202, 978, 142),-- 'The Chop Shop Collection'
	(203, 978, 142),-- 'The Overpass Collection'
	(204, 978, 135),-- 'The Rising Sun Collection'
	(205, 978, 264),-- 'The Assault Collection'
	(206, 978, 47),-- 'The Baggage Collection'
	(207, 978, 66),-- 'The Cache Collection'
	(208, 978, 47),-- 'The Safehouse Collection'
	(209, 978, 135),-- 'The Militia Collection'
	(210, 978, 28),-- 'The Dust 2 Collection'
	(211, 978, 19),-- 'The Bank Collection'
	(212, 978, 135),-- 'The Mirage Collection'
	(213, 978, 135),-- 'The Nuke Collection'
	(214, 978, 142),-- 'The Dust Collection'
	(215, 978, 203),-- 'The Office Collection'
	(216, 978, 28),-- 'The Italy Collection'
	(217, 978, 67),-- 'The Inferno Collection'
	(218, 978, 135),-- 'The Vertigo Collection'
	(219, 978, 47),-- 'The Aztec Collection'
	(220, 978, 25),-- 'The Train Collection'
	(221, 978, 25),-- 'The Lake Collection'
	(222, 978, 142),-- 'The Alpha Collection'

	-- (USD, 840)
    (10, 840, 199),-- 'Chroma Case' 1.99$
    (11, 840, 199),-- 'Phoenix Case' 1.99$
    (12, 840, 199),-- 'Breakout Case' 1.99$
    (13, 840, 199),-- 'Vanguard Case' 1.99$
    (14, 840, 255),-- 'Operation Bravo' 2.99$
    (15, 840, 199),-- 'Chroma 2 Case' 1.99$
    (16, 840, 199),-- 'Winter Offensive' 1.99$
    (17, 840, 199),-- 'Huntsman Case' 1.99$
    (18, 840, 199),-- 'Оружейный кейс: Тираж #2' 1.99$
    (19, 840, 199),-- 'Оружейный кейс: Тираж #3' 1.99$
    (20, 840, 255),-- 'eSports 2013' 2.99$
    (21, 840, 199),-- 'eSports 2013 Winter' 1.99$
    (22, 840, 199),-- 'eSports 2014 Summer' 1.99$

    (23, 840, 199),-- 'Falchion Case' 1.99$

	(24, 840, 199),-- 'Wildfire Case' 1.99$
	(25, 840, 199),-- 'Revolver Case' 1.99$
	(26, 840, 199),-- 'Shadow Case' 1.99$
	(27, 840, 199),-- 'Chroma 3 Case' 1.99$
	(28, 840, 199),-- 'Gamma Case' 1.99$
	(1000, 840, 255),-- 'Gamma 2 Case'
	(1001, 840, 255),-- 'Glove Case'

    (29, 840, 30),-- 'Consumer Raffle' 0.3$
    (30, 840, 699),-- 'Classified Raffle' 6.99$
    (31, 840, 999),-- 'Covert Raffle' 9.99$
    (32, 840, 7500),-- 'Knife Raffle' 99$

    (40, 840, 1299),-- 'Stat Track Raffle' 12.99$
    (50, 840, 12000),-- 'Dragon Lore Raffle' 99$
    (60, 840, 170),-- 'Crystal Case' 1.70$
    (61, 840, 0),-- 'Twitch Case'
    (62, 840, 170),-- 'Crystal2 Case' 1.70$

	(70, 840, 480),-- 'M4A1-S'
	(71, 840, 480),-- 'M4A4'
	(72, 840, 480),-- 'AK-47'
	(73, 840, 480),-- 'AWP'
	(74, 840, 220),-- 'Desert Eagle'

--	(80, 840, 400),-- 'Quantum Case'
--	(81, 840, 400),-- 'fEss Case'
--	(82, 840, 400),-- 'Dimonsterus Case'
--	(83, 840, 400),-- 'Nyaruko Game Case'
--	(84, 840, 400),-- 'Mr.Obzorka Case'

	(100, 840, 0),-- 'Rank Silver Case'
	(101, 840, 0),-- 'Rank Gold Case'
	(102, 840, 0),-- 'Rank Platinum Case'

	(120, 840, 0),-- 'Promo Case'
	(121, 840, 200),-- 'Shurigina Case'

	(200, 840, 200),-- 'The Cobblestone Collection'
	(201, 840, 215),-- 'The Gods And Monsters Collection'
	(202, 840, 150),-- 'The Chop Shop Collection'
	(203, 840, 150),-- 'The Overpass Collection'
	(204, 840, 143),-- 'The Rising Sun Collection'
	(205, 840, 280),-- 'The Assault Collection'
	(206, 840, 50),-- 'The Baggage Collection'
	(207, 840, 70),-- 'The Cache Collection'
	(208, 840, 50),-- 'The Safehouse Collection'
	(209, 840, 143),-- 'The Militia Collection'
	(210, 840, 30),-- 'The Dust 2 Collection'
	(211, 840, 20),-- 'The Bank Collection'
	(212, 840, 143),-- 'The Mirage Collection'
	(213, 840, 143),-- 'The Nuke Collection'
	(214, 840, 150),-- 'The Dust Collection'
	(215, 840, 215),-- 'The Office Collection'
	(216, 840, 30),-- 'The Italy Collection'
	(217, 840, 71),-- 'The Inferno Collection'
	(218, 840, 143),-- 'The Vertigo Collection'
	(219, 840, 50),-- 'The Aztec Collection'
	(220, 840, 27),-- 'The Train Collection'
	(221, 840, 27),-- 'The Lake Collection'
	(222, 840, 150);-- 'The Alpha Collection'
UNLOCK TABLES;



LOCK TABLES `iso3166` WRITE;
INSERT INTO `iso3166` VALUES
(4,		'Afghanistan',	'en', 'AF', 'AFG', 'USD',	840),
(8,		'Albania',		'en', 'AL', 'ALB', 'USD',	840),
(10,	'Antarctica',	'en', 'AQ', 'ATA', 'USD',	840),
(12,	'Algeria',		'en', 'DZ', 'DZA', 'USD',	840),
(16,	'American Samoa', 'en', 'AS', 'ASM', 'USD',	840),
(20,	'Andorra',		'en', 'AD', 'AND', 'USD',	840),
(24,	'Angola',		'en', 'AO', 'AGO', 'USD',	840),
(28,	'Antigua and Barbuda', 'en','AG', 'ATG',	'USD',	840),
(31,	'Azerbaijan',	'ru', 'AZ', 'AZE', 'RUB',	643),
(32,	'Argentina',	'en', 'AR', 'ARG', 'USD',	840),
(36,	'Australia',	'en', 'AU', 'AUS', 'USD',	840),
(40,	'Austria',		'en', 'AT', 'AUT', 'EUR',	978),
(44,	'Bahamas',		'en', 'BS', 'BHS', 'USD',	840),
(48,	'Bahrain',		'en', 'BH', 'BHR', 'USD',	840),
(50,	'Bangladesh',	'en', 'BD', 'BGD', 'USD',	840),
(51,	'Armenia',		'ru', 'AM', 'ARM', 'RUB',	643),
(52,	'Barbados',		'en', 'BB', 'BRB', 'USD',	840),
(56,	'Belgium',		'en', 'BE', 'BEL', 'EUR',	978),
(60,	'Bermuda',		'en', 'BM', 'BMU', 'USD',	840),
(64,	'Bhutan',		'en', 'BT', 'BTN', 'USD',	840),
(68,	'Bolivia',		'en', 'BO', 'BOL', 'USD',	840),
(70,	'Bosnia and Herzegovina', 'en', 'BA',	'BIH', 'USD', 840),
(72,	'Botswana',		'en', 'BW', 'BWA', 'USD', 840),
(74,	'Bouvet Island','en', 'BV', 'BVT', 'USD', 840),
(76,	'Brazil',		'en', 'BR', 'BRA', 'USD', 840),
(84,	'Belize',		'en', 'BZ', 'BLZ', 'USD', 840),
(86,	'British Indian Ocean Territory', 'en', 'IO', 'IOT', 'USD', 840),
(90,	'Solomon Islands', 'en', 'SB', 'SLB',	'USD', 840),
(92,	'Virgin Islands (British)',	'en', 'VG', 'VGB', 'USD',	840),
(96,	'Brunei Darussalam', 'en', 'BN', 'BRN', 'USD', 840),
(100,	'Bulgaria',		'en', 'BG', 'BGR', 'EUR',	978),
(104,	'Myanmar',		'en', 'MM', 'MMR', 'USD',	840),
(108,	'Burundi',		'en', 'BI', 'BDI', 'USD',	840),
(112,	'Belarus',		'ru', 'BY', 'BLR', 'RUB',	643),
(116,	'Cambodia',		'en', 'KH', 'KHM', 'USD',	840),
(120,	'Cameroon',		'en', 'CM', 'CMR', 'USD',	840),
(124,	'Canada',		'en', 'CA', 'CAN', 'USD',	840),
(132,	'Cabo Verde',	'en', 'CV', 'CPV', 'USD',	840),
(136,	'Cayman Islands', 'en', 'KY', 'CYM', 'USD', 840),
(140,	'Central African Republic',	'en', 'CF',	'CAF',	'USD',	840),
(144,	'Sri Lanka',	'en', 'LK', 'LKA', 'USD',	840),
(148,	'Chad',			'en', 'TD', 'TCD', 'USD',	840),
(152,	'Chile',		'en', 'CL', 'CHL', 'USD',	840),
(156,	'China',		'en', 'CN', 'CHN', 'USD',	840),
(158,	'Taiwan (Province of China)', 'en', 'TW', 'TWN', 'USD', 840),
(162,	'Christmas Island',	'en', 'CX', 'CXR', 'USD', 840),
(166,	'Cocos (Keeling) Islands', 'en', 'CC', 'CCK', 'USD', 840),
(170,	'Colombia',		'en', 'CO', 'COL', 'USD', 840),
(174,	'Comoros',		'en', 'KM', 'COM', 'USD', 840),
(175,	'Mayotte',		'en', 'YT', 'MYT', 'USD', 840),
(178,	'Congo',		'en', 'CG', 'COG', 'USD', 840),
(180,	'Congo',		'en', 'CD', 'COD', 'USD', 840),
(184,	'Cook Islands',	'en', 'CK', 'COK', 'USD',	840),
(188,	'Costa Rica',	'en', 'CR', 'CRI', 'USD',	840),
(191,	'Croatia',		'en', 'HR', 'HRV', 'EUR',	978),
(192,	'Cuba',			'en', 'CU', 'CUB', 'USD',	840),
(196,	'Cyprus',		'en', 'CY', 'CYP', 'EUR',	978),
(203,	'Czech Republic', 'en', 'CZ', 'CZE', 'EUR', 978),
(204,	'Benin',		'en', 'BJ', 'BEN', 'USD', 840),
(208,	'Denmark',		'en', 'DK', 'DNK', 'EUR', 978),
(212,	'Dominica',		'en', 'DM', 'DMA', 'USD', 840),
(214,	'Dominican Republic', 'en', 'DO', 'DOM', 'USD', 840),
(218,	'Ecuador',		'en', 'EC', 'ECU', 'USD', 840),
(222,	'El Salvador',	'en', 'SV', 'SLV', 'USD',	840),
(226,	'Equatorial Guinea', 'en', 'GQ', 'GNQ', 'USD', 840),
(231,	'Ethiopia',		'en', 'ET', 'ETH', 'USD', 840),
(232,	'Eritrea',		'en', 'ER', 'ERI', 'USD', 840),
(233,	'Estonia',		'en', 'EE', 'EST', 'EUR', 978),
(234,	'Faroe Islands','en', 'FO', 'FRO', 'USD', 840),
(238,	'Falkland Islands [Malvinas]', 'en', 'FK', 'FLK', 'USD', 840),
(239,	'South Georgia and the South Sandwich Islands', 'en', 'GS', 'SGS', 'USD', 840),
(242,	'Fiji',			'en', 'FJ', 'FJI', 'USD', 840),
(246,	'Finland',		'en', 'FI', 'FIN', 'EUR', 978),
(248,	'Åland Islands','en', 'AX', 'ALA', 'USD',	840),
(250,	'France',		'en', 'FR', 'FRA', 'EUR', 978),
(254,	'French Guiana','en', 'GF', 'GUF', 'USD',	840),
(258,	'French Polynesia', 'en', 'PF', 'PYF', 'USD',	840),
(260,	'French Southern Territories', 'en', 'TF', 'ATF', 'USD', 840),
(262,	'Djibouti',		'en', 'DJ', 'DJI', 'USD', 840),
(266,	'Gabon',		'en', 'GA', 'GAB', 'USD', 840),
(268,	'Georgia',		'ru', 'GE', 'GEO', 'RUB', 643),
(270,	'Gambia',		'en', 'GM', 'GMB', 'USD', 840),
(275,	'Palestine',	'en', 'PS', 'PSE', 'USD', 840),
(276,	'Germany',		'en', 'DE', 'DEU', 'EUR', 978),
(288,	'Ghana',		'en', 'GH', 'GHA', 'USD', 840),
(292,	'Gibraltar',	'en', 'GI', 'GIB', 'USD', 840),
(296,	'Kiribati',		'en', 'KI', 'KIR', 'USD', 840),
(300,	'Greece',		'en', 'GR', 'GRC', 'EUR', 978),
(304,	'Greenland',	'en', 'GL', 'GRL', 'USD', 840),
(308,	'Grenada',		'en', 'GD', 'GRD', 'USD', 840),
(312,	'Guadeloupe',	'en', 'GP', 'GLP', 'USD', 840),
(316,	'Guam',			'en', 'GU', 'GUM', 'USD', 840),
(320,	'Guatemala',	'en', 'GT', 'GTM', 'USD', 840),
(324,	'Guinea',		'en', 'GN', 'GIN', 'USD', 840),
(328,	'Guyana',		'en', 'GY', 'GUY', 'USD', 840),
(332,	'Haiti',		'en', 'HT', 'HTI', 'USD', 840),
(334,	'Heard Island and McDonald Islands', 'en', 'HM', 'HMD', 'USD', 840),
(336,	'Holy See',		'en', 'VA', 'VAT', 'USD', 840),
(340,	'Honduras',		'en', 'HN', 'HND', 'USD', 840),
(344,	'Hong Kong',	'en', 'HK', 'HKG', 'USD', 840),
(348,	'Hungary',		'en', 'HU', 'HUN', 'EUR', 978),
(352,	'Iceland',		'en', 'IS', 'ISL', 'USD', 840),
(356,	'India',		'en', 'IN', 'IND', 'USD', 840),
(360,	'Indonesia',	'en', 'ID', 'IDN', 'USD', 840),
(364,	'Iran',			'en', 'IR', 'IRN', 'USD', 840),
(368,	'Iraq',			'en', 'IQ', 'IRQ', 'USD', 840),
(372,	'Ireland',		'en', 'IE', 'IRL', 'EUR', 978),
(376,	'Israel',		'en', 'IL', 'ISR', 'USD', 840),
(380,	'Italy',		'en', 'IT', 'ITA', 'EUR', 978),
(384,	'Côte d\'Ivoire','en', 'CI', 'CIV', 'USD', 840),
(388,	'Jamaica',		'en', 'JM', 'JAM', 'USD', 840),
(392,	'Japan',		'en', 'JP', 'JPN', 'USD', 840),
(398,	'Kazakhstan',	'ru', 'KZ', 'KAZ', 'RUB', 643),
(400,	'Jordan',		'en', 'JO', 'JOR', 'USD', 840),
(404,	'Kenya',		'en', 'KE', 'KEN', 'USD', 840),
(408,	'Korea (the Democratic People\'s Republic of Korea)', 'en', 'KP', 'PRK', 'USD', 840),
(410,	'Korea (the Republic of)', 'en', 'KR', 'KOR', 'USD', 840),
(414,	'Kuwait', 		'en', 'KW', 'KWT', 'USD', 840),
(417,	'Kyrgyzstan',	'ru', 'KG', 'KGZ', 'RUB', 643),
(418,	'Lao People\'s Democratic Republic', 'en', 'LA', 'LAO', 'USD', 840),
(422,	'Lebanon',		'en', 'LB', 'LBN', 'USD', 840),
(426,	'Lesotho',		'en', 'LS', 'LSO', 'USD', 840),
(428,	'Latvia',		'en', 'LV', 'LVA', 'EUR', 978),
(430,	'Liberia',		'en', 'LR', 'LBR', 'USD', 840),
(434,	'Libya',		'en', 'LY', 'LBY', 'USD', 840),
(438,	'Liechtenstein','en', 'LI', 'LIE', 'USD', 840),
(440,	'Lietuva',		'en', 'LT', 'LTU', 'EUR', 978),
(442,	'Luxembourg',	'en', 'LU', 'LUX', 'EUR', 978),
(446,	'Macao',		'en', 'MO', 'MAC', 'USD', 840),
(450,	'Madagascar',	'en', 'MG', 'MDG', 'USD', 840),
(454,	'Malawi',		'en', 'MW', 'MWI', 'USD', 840),
(458,	'Malaysia',		'en', 'MY', 'MYS', 'USD', 840),
(462,	'Maldives',		'en', 'MV', 'MDV', 'USD', 840),
(466,	'Mali',			'en', 'ML', 'MLI', 'USD', 840),
(470,	'Malta',		'en', 'MT', 'MLT', 'EUR', 978),
(474,	'Martinique',	'en', 'MQ', 'MTQ', 'USD', 840),
(478,	'Mauritania',	'en', 'MR', 'MRT', 'USD', 840),
(480,	'Mauritius',	'en', 'MU', 'MUS', 'USD', 840),
(484,	'Mexico',		'en', 'MX', 'MEX', 'USD', 840),
(492,	'Monaco',		'en', 'MC', 'MCO', 'USD', 840),
(496,	'Mongolia',		'en', 'MN', 'MNG', 'USD', 840),
(498,	'Moldova', 		'en', 'MD', 'MDA', 'USD', 840),
(499,	'Montenegro',	'en', 'ME', 'MNE', 'USD', 840),
(500,	'Montserrat',	'en', 'MS', 'MSR', 'USD', 840),
(504,	'Morocco',		'en', 'MA', 'MAR', 'USD', 840),
(508,	'Mozambique',	'en', 'MZ', 'MOZ', 'USD', 840),
(512,	'Oman',			'en', 'OM', 'OMN', 'USD', 840),
(516,	'Namibia',		'en', 'NA', 'NAM', 'USD', 840),
(520,	'Nauru',		'en', 'NR', 'NRU', 'USD', 840),
(524,	'Nepal',		'en', 'NP', 'NPL', 'USD', 840),
(528,	'Netherlands',	'en', 'NL', 'NLD', 'EUR', 978),
(531,	'Curaçao',		'en', 'CW', 'CUW', 'USD', 840),
(533,	'Aruba',		'en', 'AW', 'ABW', 'USD', 840),
(534,	'Sint Maarten (Dutch part)', 'en', 'SX', 'SXM', 'USD', 840),
(535,	'Bonaire, Sint Eustatius and Saba', 'en', 'BQ', 'BES', 'USD', 840),
(540,	'New Caledonia','en', 'NC', 'NCL', 'USD', 840),
(548,	'Vanuatu',		'en', 'VU', 'VUT', 'USD', 840),
(554,	'New Zealand',	'en', 'NZ', 'NZL', 'USD', 840),
(558,	'Nicaragua',	'en', 'NI', 'NIC', 'USD', 840),
(562,	'Niger',		'en', 'NE', 'NER', 'USD', 840),
(566,	'Nigeria',		'en', 'NG', 'NGA', 'USD', 840),
(570,	'Niue',			'en', 'NU', 'NIU', 'USD', 840),
(574,	'Norfolk Island', 'en', 'NF', 'NFK', 'USD', 840),
(578,	'Norway',		'en', 'NO', 'NOR', 'USD', 840),
(580,	'Northern Mariana Islands',	'en', 'MP', 'MNP', 'USD', 840),
(581,	'United States Minor Outlying Islands',	'en', 'UM', 'UMI', 'USD', 840),
(583,	'Micronesia (Federated States)', 'en', 'FM', 'FSM', 'USD', 840),
(584,	'Marshall Islands', 'en', 'MH', 'MHL', 'USD', 840),
(585,	'Palau',		'en', 'PW', 'PLW', 'USD', 840),
(586,	'Pakistan',		'en', 'PK', 'PAK', 'USD', 840),
(591,	'Panama',		'en', 'PA', 'PAN', 'USD', 840),
(598,	'Papua New Guinea', 'en', 'PG', 'PNG', 'USD', 840),
(600,	'Paraguay',		'en', 'PY', 'PRY', 'USD', 840),
(604,	'Peru',			'en', 'PE', 'PER', 'USD', 840),
(608,	'Philippines',	'en', 'PH', 'PHL', 'USD', 840),
(612,	'Pitcairn',		'en', 'PN', 'PCN', 'USD', 840),
(616,	'Poland',		'en', 'PL', 'POL', 'EUR', 978),
(620,	'Portugal',		'en', 'PT', 'PRT', 'EUR', 978),
(624,	'Guinea-Bissau','en', 'GW', 'GNB', 'USD', 840),
(626,	'Timor-Leste',	'en', 'TL', 'TLS', 'USD', 840),
(630,	'Puerto Rico',	'en', 'PR', 'PRI', 'USD', 840),
(634,	'Qatar',		'en', 'QA', 'QAT', 'USD', 840),
(638,	'Réunion',		'en', 'RE', 'REU', 'USD', 840),
(642,	'Romania',		'en', 'RO', 'ROU', 'EUR', 978),
(643,	'Russian Federation', 'ru', 'RU', 'RUS', 'RUB', 643),
(646,	'Rwanda', 		'en', 'RW', 'RWA', 'USD', 840),
(652,	'Saint Barthélemy',	'en', 'BL', 'BLM', 'USD', 840),
(654,	'Saint Helena, Ascension and Tristan da Cunha', 'en', 'SH', 'SHN', 'USD', 840),
(659,	'Saint Kitts and Nevis', 'en', 'KN', 'KNA', 'USD', 840),
(660,	'Anguilla',		'en', 'AI', 'AIA', 'USD', 840),
(662,	'Saint Lucia',	'en', 'LC', 'LCA', 'USD', 840),
(663,	'Saint Martin (French part)', 'en', 'MF', 'MAF', 'USD', 840),
(666,	'Saint Pierre and Miquelon', 'en', 'PM', 'SPM', 'USD', 840),
(670,	'Saint Vincent and the Grenadines', 'en', 'VC', 'VCT', 'USD', 840),
(674,	'San Marino',	'en', 'SM', 'SMR', 'USD', 840),
(678,	'Sao Tome and Principe', 'en', 'ST', 'STP', 'USD', 840),
(682,	'Saudi Arabia',	'en', 'SA', 'SAU', 'USD', 840),
(686,	'Senegal',		'en', 'SN', 'SEN', 'USD', 840),
(688,	'Serbia',		'en', 'RS', 'SRB', 'USD', 840),
(690,	'Seychelles',	'en', 'SC', 'SYC', 'USD', 840),
(694,	'Sierra Leone',	'en', 'SL', 'SLE', 'USD', 840),
(702,	'Singapore',	'en', 'SG', 'SGP', 'USD', 840),
(703,	'Slovakia',		'en', 'SK', 'SVK', 'EUR', 978),
(704,	'Viet Nam',		'en', 'VN', 'VNM', 'USD', 840),
(705,	'Slovenia',		'en', 'SI', 'SVN', 'EUR', 978),
(706,	'Somalia',		'en', 'SO', 'SOM', 'USD', 840),
(710,	'South Africa', 'en', 'ZA', 'ZAF', 'USD', 840),
(716,	'Zimbabwe', 	'en', 'ZW', 'ZWE', 'USD', 840),
(724,	'Spain',		'en', 'ES', 'ESP', 'EUR', 978),
(728,	'South Sudan',	'en', 'SS', 'SSD', 'USD', 840),
(729,	'Sudan',		'en', 'SD', 'SDN', 'USD', 840),
(732,	'Western Sahara', 'en', 'EH', 'ESH', 'USD', 840),
(740,	'Suriname',		'en', 'SR', 'SUR', 'USD', 840),
(744,	'Svalbard and Jan Mayen', 'en', 'SJ', 'SJM', 'USD', 840),
(748,	'Swaziland',	'en', 'SZ', 'SWZ', 'USD', 840),
(752,	'Sweden',		'en', 'SE', 'SWE', 'EUR', 978),
(756,	'Switzerland',	'en', 'CH', 'CHE', 'USD', 840),
(760,	'Syrian Arab Republic', 'en', 'SY', 'SYR', 'USD', 840),
(762,	'Tajikistan',	'en', 'TJ', 'TJK', 'USD', 840),
(764,	'Thailand',		'en', 'TH', 'THA', 'USD', 840),
(768,	'Togo',			'en', 'TG', 'TGO', 'USD', 840),
(772,	'Tokelau',		'en', 'TK', 'TKL', 'USD', 840),
(776,	'Tonga',		'en', 'TO', 'TON', 'USD', 840),
(780,	'Trinidad and Tobago', 'en', 'TT', 'TTO', 'USD', 840),
(784,	'United Arab Emirates', 'en', 'AE', 'ARE', 'USD', 840),
(788,	'Tunisia',		'en', 'TN', 'TUN', 'USD', 840),
(792,	'Turkey',		'en', 'TR', 'TUR', 'USD', 840),
(795,	'Turkmenistan',	'en', 'TM', 'TKM', 'USD', 840),
(796,	'Turks and Caicos Islands',	'en', 'TC', 'TCA', 'USD', 840),
(798,	'Tuvalu',		'en', 'TV', 'TUV', 'USD', 840),
(800,	'Uganda',		'en', 'UG', 'UGA', 'USD', 840),
(804,	'Ukraine',		'ru', 'UA', 'UKR', 'RUB', 643),
(807,	'Macedonia (the former Yugoslav Republic)',	'en', 'MK', 'MKD', 'USD', 840),
(818,	'Egypt',		'en', 'EG', 'EGY', 'USD', 840),
(826,	'United Kingdom of Great Britain and Northern Ireland', 'en', 'GB', 'GBR', 'EUR', 978),
(831,	'Guernsey',		'en', 'GG', 'GGY', 'USD', 840),
(832,	'Jersey',		'en', 'JE', 'JEY', 'USD', 840),
(833,	'Isle of Man',	'en', 'IM', 'IMN', 'USD', 840),
(834,	'Tanzania, United Republic', 'en', 'TZ', 'TZA', 'USD', 840),
(840,	'United States of America',	'en', 'US', 'USA', 'USD', 840),
(850,	'Virgin Islands (U.S.)', 'en', 'VI', 'VIR', 'USD', 840),
(854,	'Burkina Faso',	'en', 'BF', 'BFA', 'USD', 840),
(858,	'Uruguay',		'en', 'UY', 'URY', 'USD', 840),
(860,	'Uzbekistan',	'en', 'UZ', 'UZB', 'USD', 840),
(862,	'Venezuela (Bolivarian Republic)', 'en', 'VE', 'VEN', 'USD', 840),
(876,	'Wallis and Futuna', 'en', 'WF', 'WLF', 'USD', 840),
(882,	'Samoa',		'en', 'WS',	'WSM',	'USD', 840),
(887,	'Yemen',		'en', 'YE',	'YEM',	'USD', 840),
(894,	'Zambia',		'en', 'ZM',	'ZMB',	'USD', 840);

UNLOCK TABLES;



INSERT INTO log_desc (log_desc_id) VALUES(1);
INSERT INTO log_desc (log_desc_id) VALUES(2);

UPDATE log_desc SET template_ru = "пользователь &quot;{{ username }}&quot; авторизировался {{ date }} с {{ ip }}" WHERE log_desc_id = 1;
UPDATE log_desc SET template_ru = "пользователь &quot;{{ username }}&quot; сменил статус предметов {{ date }} с {{ ip }}.<br>новый статус: {{ status }}.<br>список предметов:<br> {% for item in itemlist %} {{item.market_hash_name}} {{ item.price }} <br>{% endfor %}" WHERE log_desc_id = 2;





LOCK TABLES `ip2location_v4_db1` WRITE;
LOAD DATA LOCAL
	-- INFILE '/home/kraysx7/projects/clodex_project/cbsystem/cbserver/sql/ip2location_db1/01.11.2016/IPCountry.csv'
	INFILE '/usr/local/src/cbsystem/cbserver/sql/ip2location_db1/01.11.2016/IPCountry.csv'
INTO TABLE
	`ip2location_v4_db1`
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\r\n'
IGNORE 0 LINES;
UNLOCK TABLES;





-- Таблица тестовых лотов для аукциона (ПРОВЕРИТЬ И УБРАТЬ !!!)
-- LOCK TABLES `user_service` WRITE;

-- INSERT INTO `promocode` VALUES (NULL, `in_user_id`, `in_service_id`, `in_node_id`, `in_params`, `in_create_date`, `in_end_date`, `in_status`);

-- INSERT INTO `promocode` VALUES (NULL, 1, 'Bigstream', '[{points, 100}].', '2017-03-21 03:59:00', 0, 100, 0);
-- INSERT INTO `promocode` VALUES (NULL, 1, 'Addthis', '[{points, 100}].', '2017-03-21 03:59:00', 0, 100, 0);
-- INSERT INTO `promocode` VALUES (NULL, 1, 'donatetous', '[{points, 100}].', '2017-03-21 03:59:00', 0, 100, 0);

-- INSERT INTO `promocode` VALUES (NULL, 1, 'sob-kov', '[].', ' 2017-02-14 03:59:00 ', 0, 400, 0);
-- INSERT INTO `promocode` VALUES (NULL, 3, 'RP-400', '[{service_id, 120}].', ' 2017-02-28 23:59:00 ', 0, 400, 0);

-- INSERT INTO `promocode` VALUES (NULL, 3, 'ANTON', '[{service_id, 120}].', ' 2017-03-05 23:59:00 ', 0, 20, 0);


-- INSERT INTO `user_service` VALUES (NULL, `in_user_id`, `in_service_id`, `in_node_id`, `in_params`, `in_create_date`, `in_end_date`, `in_status`);
-- INSERT INTO `user_service` VALUES 
--	(100, 0, 2000, 0, '[{item_name_ru, "★ Нож с лезвием-крюком | Пиксельный камуфляж «Лес»"}, {item_name_en, "Forest DDPAT"}, {rarity, covert}, {type, knife}, {exterior, "Field-Tested"}, {cur_price, 2450.54}, {steam_price, 2650.54}, {image, "fWFc82js0fmoRAP-qOIPu5THSWqfSmTELLqcUywGkijVjZYMUrsm1j-9xgEObwgfEh_nvjlWhNzZCveCDfIBj98xqodQ2CZknz5wOuqzNQhqKzvAALlRUvAuywD1NiE9-sJwQOi0-KkDKFCA6NuRa_RDbIkOSJXOWvGCNw6s401piKZYfpCO9Sy-iSzvb2ZbXRTj-G8BzuSE77U_hmlEEW_w87uLpmm_PQ"}].', NULL, NULL, 0);
    
-- UNLOCK TABLES;


