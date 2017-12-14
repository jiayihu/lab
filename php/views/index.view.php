<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta http-equiv="X-UA-Compatible" content="ie=edge">
  <title>Learning PhP</title>
</head>
<body>
  <ul>
    <?php foreach ($tasks as $task) : ?>
      <li>
        <?php
          if ($task->isComplete()) {
            echo "<strike>{$task->description}</strike>";
          } else {
            echo $task->description;
          }
        ?>
      </li>
    <?php endforeach; ?>
  </ul>
</body>
</html>
