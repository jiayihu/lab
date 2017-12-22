<?php

function view($name, $data = []) {
  extract($data);
  
  return require_once "views/{$name}.view.php";
}

function redirect($path) {
  header("Location: /{$path}");
}
