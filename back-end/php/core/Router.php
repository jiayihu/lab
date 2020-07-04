<?php

namespace Core;

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
      $route = \explode('@', $this->routes[$method][$uri]);
      $controller = $route[0];
      $action = $route[1];

      return $this->callAction($controller, $action);
    } else {
      return $this->routes['GET']['404'];
    }
  }

  private function callAction($controller, $action) {
    require_once "app/controllers/{$controller}.php";

    $name = "App\\Controllers\\{$controller}";
    $instance = new $name;

    if (!\method_exists($instance, $action)) {
      throw new Exception("{$action} does not exist on controller {$controller}");
    }

    return $instance->$action();
  }

  private function parseRoutes($routes) {
    if (\array_key_exists('GET', $routes)) {
      foreach ($routes['GET'] as $uri => $controller) {
        $this->get($uri, $controller);
      }
    }

    if (\array_key_exists('POST', $routes)) {
      foreach ($routes['POST'] as $uri => $controller) {
        $this->post($uri, $controller);
      }
    }
  }
}
