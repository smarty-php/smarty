<?

require("Smarty.class.php");

$smarty = new Smarty;

$smarty->assign("Name","Fred");
$smarty->assign("FirstName",array("John","Mary","James","Henry"));
$smarty->assign("LastName",array("Doe","Smith","Johnson","Case"));
$smarty->assign("Class",array("A","B","C","D"));

$smarty->spew("./templates/index.tpl");


?>
