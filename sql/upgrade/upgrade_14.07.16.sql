ALTER TABLE `user` ADD `currency_alpha`  CHAR(3) NOT NULL AFTER `balance`;
ALTER TABLE `user` ADD `currency_number` INT 	 NOT NULL AFTER `currency_alpha`;
ALTER TABLE `user` ADD `country_number`  INT 	 NOT NULL AFTER `currency_number`;

ALTER TABLE `user` ADD `last_ip_alpha`	 CHAR(32) DEFAULT NULL AFTER `last_ip`;

ALTER TABLE `user` MODIFY `last_ip` BIGINT DEFAULT 0;

-- ALTER TABLE `transaction` MODIFY `currency_alpha` CHAR(5) NOT NULL;

ALTER TABLE `transaction` ADD `currency_alpha` CHAR(5)	NOT NULL AFTER `user_id`;			-- буквенный код валюты
ALTER TABLE `transaction` ADD `currency_number` INT		NOT NULL AFTER `currency_alpha`;			-- цифровой код валюты
