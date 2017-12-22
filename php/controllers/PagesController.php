<?php 

require_once 'controllers/UsersController.php';

class PagesController {
  public function home() {
    $tasks = (new UsersController())->getTasksTemplate();

    return view('index', ['tasks' => $tasks]);
  }
  
  public function about() {
    view('about');
  }

  public function notFound() {

  }
}
