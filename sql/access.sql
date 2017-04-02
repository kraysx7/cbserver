USE `cloudbilling`;

-- --------------------------------------------------------
-- Таблица для хранения прав доступа
-- --------------------------------------------------------
DROP TABLE IF EXISTS `access`;
CREATE TABLE `access` (
        `user_id`                 BIGINT         NOT NULL,                -- идентификатор пользователя в локальной системе
	`controller`              VARCHAR(265)   NOT NULL,                -- имя контроллера разрешенного действия
        `action`        	  VARCHAR(265)   NOT NULL,                -- имя действия
	`closure`		  INT            NOT NULL                 -- номер кляузы
) ENGINE=InnoDB  DEFAULT CHARSET=utf8;