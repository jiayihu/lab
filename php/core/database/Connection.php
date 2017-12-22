<?php 

namespace Core\Database;

class Connection {
  public static function make($config) {
    try {
      return new \PDO(
        "mysql:host={$config['hostdb']};dbname={$config['name']}",
        $config['username'],
        $config['password'],
        $config['options']
      );
    } catch (\PDOException $e) {
      die('Could not connect' . $e->getMessage());
    }
  }
}
