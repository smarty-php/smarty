PHP file test
$foo is <?=$foo?>
<br> Test modifier chaining
<?=$foo->trim()->escape('html')?>
<br>Test objects
<?=$person->setName('Paul')->setAge(39)->introduce()->trim()->truncate(10)?>
<br>Arrays
<?=$array['a']['aa']->truncate(5)?><?=$array['b']?>
<br>Function
<?=$_f->trim($array['a']['aa'])->truncate(10)?>
DONE
