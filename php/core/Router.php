<?php

class Router {
  private $routes = [];

  public static function load($file) {
    $routes = require $file;
    $router = new self($routes);

    return $router;
  }

  public function __construct($routes) {
    $this->routes = $routes;
  }

  public function define($routes) {
    $this->routes = $routes;
  }

  public function direct($uri) {
    if (array_key_exists($uri, $this->routes)) {
      return $this->routes[$uri];
    } else {
      return $this->routes['404'];
    }
  }
}
