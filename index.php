<?

require("Smarty.class.php");

$smarty = new Smarty;

$smarty->assign("Name","Fred");
$smarty->assign("FirstName",array("John","Mary","James","Henry"));
$smarty->assign("LastName",array("Doe","Smith","Johnson","Case"));
$smarty->assign("Class",array(array("A","B","C","D"), array("E", "F", "G", "H"),
							  array("I", "J", "K", "L"), array("M", "N", "O", "P")));

$smarty->quip("./templates/index.tpl");

print "\ndone\n";

?>
