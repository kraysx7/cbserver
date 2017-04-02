USE `cloudbilling`;

DELIMITER //

DROP PROCEDURE IF EXISTS `create_log_entry`; //
CREATE  PROCEDURE `create_log_entry`(IN `in_log_desc_id` BIGINT UNSIGNED, IN `in_log_template_params` VARCHAR(512), IN `in_user_id` BIGINT UNSIGNED, IN `in_subject_ip` VARCHAR(512))
BEGIN
	INSERT INTO `log` (`log_desc_id`, `log_template_params`, `user_id`, `create_date`, `subject_ip`) VALUES (`in_log_desc_id`, `in_log_template_params`, `in_user_id`, NOW(), INET6_ATON(`in_subject_ip`));
	SELECT LAST_INSERT_ID() AS `log_id`;
END; //


