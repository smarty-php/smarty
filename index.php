<?

require("./Smarty.class.php");

$smarty = new Smarty;

$smarty->assign("Name","Fred Irving Johnathan Bradley Peppergill");
$smarty->assign("FirstName",array("John","Mary","James","Henry"));
$smarty->assign("LastName",array("Doe","Smith","Johnson","Case"));
$smarty->assign("Class",array(array("A","B","C","D"), array("E", "F", "G", "H"),
							  array("I", "J", "K", "L"), array("M", "N", "O", "P")));

$smarty->display("./templates/index.tpl");


function test_insert()
{
	print "<br>this is a test for the insert function<br>\n";

}
?>
