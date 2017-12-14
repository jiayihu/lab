<?php

require_once 'model/Task.php';

$tasks = fetchAllTasks($database);
$tasks[0]->complete();

require 'views/index.view.php';
