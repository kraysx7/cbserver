-- ######################## ХРАНИМЫЕ ПРОЦЕДУРЫ ######################## 

-- USE `cloudbilling`;
-- USE `progameru`;
USE `tmskinscom`;

DELIMITER //


DROP PROCEDURE IF EXISTS `create_partner`; //
CREATE  PROCEDURE `create_partner`(IN `in_user_id` BIGINT, IN `in_type` TINYINT, IN `in_key` VARCHAR(32), IN `in_balance`BIGINT)
BEGIN
    INSERT INTO `partner` (`user_id`, `type`, `key`, `balance`) VALUES (`in_user_id`, `in_type`, `in_key`, `in_balance`);
	SELECT LAST_INSERT_ID() AS `partner_id`;
END; //



--	`country_number`  INT		NOT NULL,			-- цифровой код страны из таблицы `iso3166` (устанавливается при регистрации)
--	`currency_alpha`  CHAR(3)	NOT NULL,			-- буквенный код валюты из таблицы `iso3166` (устанавливается при регистрации)
--	`currency_number` INT		NOT NULL,			-- цифровой код валюты из таблицы `iso3166` (устанавливается при регистрации)

DROP PROCEDURE IF EXISTS `create_user`; //
CREATE  PROCEDURE `create_user`(IN `in_remote_user_id` BIGINT, 
								IN `in_remote_auth_type` TINYINT, 
								IN `in_user_name` VARCHAR(128),

								IN `in_currency_alpha` CHAR(3), 
								IN `in_currency_number` INT, 
								IN `in_country_number` INT, 
								 
								IN `in_password` CHAR(64), 
								IN `in_email` VARCHAR(128),
								IN `in_params` VARCHAR(2048))
BEGIN
    INSERT INTO `user` (`remote_user_id`,       `remote_auth_type`,         `name`,    `currency_alpha`,    `currency_number`,    `country_number`,    `password`, `reg_date`,    `email`,    `params`) VALUES
					   (`in_remote_user_id`, `in_remote_auth_type`, `in_user_name`, `in_currency_alpha`, `in_currency_number`, `in_country_number`, `in_password`,      NOW(), `in_email`, `in_params`);
	SELECT LAST_INSERT_ID() AS `user_id`;
END; //


DROP PROCEDURE IF EXISTS `create_transaction`; //
CREATE  PROCEDURE `create_transaction`(
								IN `in_type` INT, IN `in_params` VARCHAR(2048), IN `in_user_id` BIGINT, 
								IN `in_currency_alpha` CHAR(5), 
								IN `in_currency_number` INT,  
								IN `in_cost` INT, 
								IN `in_new_balance` INT, 
								IN `in_status` TINYINT, 
								IN `in_create_date` DATETIME, 
								IN `in_close_date` DATETIME)
BEGIN
	INSERT INTO `transaction` VALUES (NULL, `in_type`, `in_params`, `in_user_id`, `in_currency_alpha`, `in_currency_number`, `in_cost`, `in_new_balance`, `in_status`, `in_create_date`, `in_close_date`);
	SELECT LAST_INSERT_ID() AS `transaction_id`;
END; //


DROP PROCEDURE IF EXISTS `create_order`; //
CREATE  PROCEDURE `create_order`(IN `in_user_id` INT, IN `in_service_id` INT, IN `in_params` VARCHAR(2048), IN `in_period` INT, IN `in_status` INT, IN `in_processing_mode` INT, IN `in_create_date` DATETIME, IN `in_close_date` DATETIME)
BEGIN
    INSERT INTO `order` VALUES (NULL,`in_user_id`,`in_service_id`, `in_params`, `in_period`,`in_status`,`in_processing_mode`,`in_create_date`,`in_close_date`);
	SELECT LAST_INSERT_ID() AS `order_id`;
END; //


DROP PROCEDURE IF EXISTS `create_user_service`; //
CREATE  PROCEDURE `create_user_service`(IN `in_user_id` BIGINT, IN `in_service_id` INT, IN `in_node_id` INT, IN `in_params` VARCHAR(2048), IN `in_create_date` DATETIME, IN `in_end_date` DATETIME, IN `in_status` TINYINT)
BEGIN
	INSERT INTO `user_service` VALUES (NULL, `in_user_id`, `in_service_id`, `in_node_id`, `in_params`, `in_create_date`, `in_end_date`, `in_status`);
	SELECT LAST_INSERT_ID() AS `user_service_id`;
END; //


DROP PROCEDURE IF EXISTS `create_log_entry`; //
CREATE  PROCEDURE `create_log_entry`(IN `in_log_desc_id` BIGINT UNSIGNED, IN `in_log_template_params` VARCHAR(512), IN `in_user_id` BIGINT UNSIGNED, IN `in_subject_ip` VARCHAR(512))
BEGIN
	INSERT INTO `log` (`log_desc_id`, `log_template_params`, `user_id`, `create_date`, `subject_ip`) VALUES (`in_log_desc_id`, `in_log_template_params`, `in_user_id`, NOW(), INET6_ATON(`in_subject_ip`));
	SELECT LAST_INSERT_ID() AS `log_id`;
END; //


DROP PROCEDURE IF EXISTS `create_user_promocode`; //
CREATE  PROCEDURE `create_user_promocode`(IN `in_promocode_id` BIGINT, IN `in_user_id` BIGINT, IN `in_params` VARCHAR(2048), IN `in_activate_date` DATETIME, IN `in_activate_ip` BIGINT, IN `in_activate_ip_alpha`	CHAR(32)	, IN `in_status` TINYINT)
BEGIN
	INSERT INTO `user_promocode` VALUES (NULL, `in_promocode_id`, `in_user_id`, `in_params`, `in_activate_date`, `in_activate_ip`, `in_activate_ip_alpha`, `in_status`);
	SELECT LAST_INSERT_ID() AS `user_promocode_id`;
END; //

DELIMITER ;


