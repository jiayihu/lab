<?php

require_once 'model/Task.php';

class UsersController {
  public function index() {
    $tasks = $this->fetchAllTasks(App::get('database'));
    $tasks[0]->complete();
    
    return view('tasks', ['tasks' => $tasks]);
  }

  public function getTasksTemplate() {
    $tasks = $this->fetchAllTasks(App::get('database'));
    $tasks[0]->complete();
    
    ob_start();
    view('tasks', ['tasks' => $tasks]);
    $template = ob_get_clean();

    return $template;
  }

  private function fetchAllTasks($database) {
    $results = $database->selectAll('todos');
    
    $tasks = array_map(function($result) {
      return new Task($result->description);
    }, $results);
  
    return $tasks;
  }
}
