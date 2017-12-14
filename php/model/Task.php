<?php

class Task {
  public $description;
  private $completed = false;

  public function __construct($description) {
    $this->description = $description;
  }

  public function isComplete() {
    return $this->completed;
  }

  public function complete() {
    $this->completed = true;
  }
}

// I'll just put this function here. I do what I want Damiano.
function fetchAllTasks($queryBuilder) {
  $results = $queryBuilder->selectAll('todos');
  
  $tasks = array_map(function($result) {
    return new Task($result->description);
  }, $results);

  return $tasks;
}
