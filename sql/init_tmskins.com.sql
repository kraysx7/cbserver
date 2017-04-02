USE `tmskinscom`;


-- Таблица узлов-обработчиков (боты и др.)
LOCK TABLES `node` WRITE;
TRUNCATE TABLE `node`;
INSERT INTO `node` VALUES 
	(1,  'bot1.steam.gocs.pro', '127.0.0.1', 92000, '', 0, 0, '', 0); -- Steam Market 
--	(28, 'node128.steamcloud.progame.ru', '127.0.0.1', 3003, '', 0, 0, '', 0), -- gocs.pro Knife Raffle
--	(29, 'node129.steamcloud.progame.ru', '127.0.0.1', 3040, '', 0, 0, '', 0), -- gocs.pro Stat Trak Raffle
--	(30, 'node130.steamcloud.progame.ru', '127.0.0.1', 3050, '', 0, 0, '', 0); -- gocs.pro Dragon Lore Raffle
UNLOCK TABLES;


-- Таблица сервисов (услуг) биллингового сервера
LOCK TABLES `service` WRITE;
TRUNCATE TABLE `service`;
INSERT INTO `service` VALUES
    (2000, 'Reveal Auction CS:GO'	, 92000, '[ ].', 0, 0, 0), -- Аукцион на понижение с закрытой ценой, лоты хранятся в user_service
	(3000, 'Steam Market Trade'	, 93000, '[ ].', 0, 0, 0); -- Выставка предмета Steam к продаже на торговой площадке
UNLOCK TABLES;


-- Таблица цен сервисов - кейсов в разных валютах
LOCK TABLES `service_cost` WRITE;
TRUNCATE TABLE `service_cost`;
INSERT INTO `service_cost` VALUES
    (2000, 643, 0), -- 'RUB Auction cost stub'
    (2000, 978, 0), -- 'EUR Auction cost stub'
    (2000, 840, 0), -- 'USD Auction cost stub'

    (3000, 643, 0), -- 'RUB Market Trade cost stub'
    (3000, 978, 0), -- 'EUR Market Trade cost stub'
    (3000, 840, 0); -- 'USD Market Tradeb cost stub'
UNLOCK TABLES;



LOCK TABLES `iso3166` WRITE;
TRUNCATE TABLE `iso3166`;
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

