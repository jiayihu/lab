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
