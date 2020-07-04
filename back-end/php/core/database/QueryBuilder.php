<?php

namespace Core\Database;

class QueryBuilder {
  private $pdo;

  public function __construct($pdo) {
    $this->pdo = $pdo;
  }

  public function selectAll($table) {
    $statement = $this->pdo->prepare("select * from {$table};");
    $statement->execute();
    
    $results = $statement->fetchAll(\PDO::FETCH_OBJ);

    return $results;
  }

  public function insert($table, $parameters) {
    $columns = \implode(', ', array_keys($parameters));
    $placeholders = \implode(
      ', ',
      \array_map(function ($key) { return ":{$key}"; }, \array_keys($parameters))
    );
    $query = printf('insert into %s (%s) values (%s)', $table, $placeholders);

    $statement = $this->pdo->prepare($sql);
    $statement->execute($parameters);
  }
}
