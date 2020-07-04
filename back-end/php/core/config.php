<?php

namespace Core;

return [
  'database' => [
    'name' => 'mytodo',
    'username'=> 'jiayi',
    'password' => 'password',
    'hostdb' => '127.0.0.1',
    'options' => [
      \PDO::ATTR_ERRMODE => \PDO::ERRMODE_EXCEPTION
    ]
  ]
];
