<?php
print('Hello');
echo "world\n";
$test = 100;
$array = ['hi', 0, 2 => 'bye', "brother has $$test dollars"];
foreach ($array as $a) {
    echo "<li>$a</li>";
}

$output=null;
$retval=null;
exec('whoami', $output, $retval);
print($output[0]);

$script = <<< JS
console.log("$test")
JS
?>

<h1>Hi</h1>
Hi again!
<script><?= $script ?></script>
<script>document.querySelector('h1').setAttribute('background', '#000');</script>
<script>console.log('hi')</script>
<h2><?php echo $retval;?></h2>
