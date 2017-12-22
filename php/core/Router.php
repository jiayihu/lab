<?php

class Router {
  private $routes = [
    'GET' => [],
    'POST' => []
  ];

  public static function load($file) {
    $routes = require $file;
    $router = new self($routes);

    return $router;
  }

  public function __construct($routes) {
    $this->parseRoutes($routes);
  }

  public function define($routes) {
    $this->parseRoutes($routes);
  }

  public function get($uri, $controller) {
    $this->routes['GET'][$uri] = $controller;
  }

  public function post($uri, $controller) {
    $this->routes['POST'][$uri] = $controller;
  }

  public function direct($uri, $method) {
    if (array_key_exists($method, $this->routes) && array_key_exists($uri, $this->routes[$method])) {
      return $this->routes[$method][$uri];
    } else {
      return $this->routes['GET']['404'];
    }
  }

  private function parseRoutes($routes) {
    if (array_key_exists('GET', $routes)) {
      foreach ($routes['GET'] as $uri => $controller) {
        $this->get($uri, $controller);
      }
    }

    if (array_key_exists('POST', $routes)) {
      foreach ($routes['POST'] as $uri => $controller) {
        $this->post($uri, $controller);
      }
    }
  }
}
