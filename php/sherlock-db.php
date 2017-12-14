<?php

/**
 * Some shit learned at university...
 */


$config = require_once 'config.php';

class DBAccess {
  public $connessione;

  public function openConnection($config) {
    $connessione = mysqli_connect(
      $config['hostdb'], 
      $config['username'], 
      $config['password'], 
      'sherlock'
    );
    $this->connessione = $connessione;

    if ($connessione) {
      $this->getListPersonaggi();
      return true;
    } else {
      die ("Error in opening DB Connection" . mysqli_connect_error());
    }
  }

  public function getListPersonaggi() {
    $query = "SELECT * from investigatore;";
    $result = mysqli_query($this->connessione, $query);

    if ($result) {
      while ($row = $result->fetch_assoc()) {
        echo '<ul>';
        printf("<li>Nome: %s, Cognome: %s</li>", $row["nome"], $row["cognome"]);
        echo '</ul>';
      }
    } else {
      die('No results');
    }
  }
}

$dbAcess = new DBAccess();

$openDBConnection = $dbAcess->openConnection($config['database']);
