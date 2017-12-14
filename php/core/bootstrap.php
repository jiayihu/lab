<?php

require_once 'core/functions.php';
$config = require_once 'core/config.php';
require_once 'core/database/Connection.php';
require_once 'core/database/QueryBuilder.php';
require_once 'core/Router.php';
require_once 'core/Request.php';

$app = [];
$app['config'] = $config;

$pdo = Connection::make($app['config']['database']);
return new QueryBuilder($pdo);
