<?php

class QueryBuilder {
  private $pdo;

  public function __construct($pdo) {
    $this->pdo = $pdo;
  }

  public function selectAll($table) {
    $statement = $this->pdo->prepare("select * from {$table};");
    $statement->execute();
    
    $results = $statement->fetchAll(PDO::FETCH_OBJ);

    return $results;
  }
}
