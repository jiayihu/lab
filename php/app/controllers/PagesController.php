<?php 

namespace App\Controllers;

require_once 'app/controllers/UsersController.php';

class PagesController {
  public function home() {
    $tasks = (new UsersController())->getTasksTemplate();

    return \Core\view('index', ['tasks' => $tasks]);
  }
  
  public function about() {
    \Core\view('about');
  }

  public function notFound() {

  }
}
