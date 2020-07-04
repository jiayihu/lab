<?php

namespace App\Controllers;
use App\Models\Task;

require_once 'app/models/Task.php';


class UsersController {
  public function index() {
    $tasks = $this->fetchAllTasks(App::get('database'));
    $tasks[0]->complete();
    
    return \Core\view('tasks', ['tasks' => $tasks]);
  }

  public function getTasksTemplate() {
    $tasks = $this->fetchAllTasks(\Core\App::get('database'));
    $tasks[0]->complete();
    
    \ob_start();
    \Core\view('tasks', ['tasks' => $tasks]);
    $template = \ob_get_clean();

    return $template;
  }

  private function fetchAllTasks($database) {
    $results = $database->selectAll('todos');
    
    $tasks = \array_map(function($result) {
      return new Task($result->description);
    }, $results);
  
    return $tasks;
  }
}
