<?

require("Smarty.class.php");

$smarty = new Smarty;

$smarty->caching = false;

$smarty->assign(now, time());

$smarty->assign("Name","Fred Irving Johnathan Bradley Peppergill");
$smarty->assign("FirstName",array("John","Mary","James","Henry"));
$smarty->assign("LastName",array("Doe","Smith","Johnson","Case"));
$smarty->assign("Class",array(array("A","B","C","D"), array("E", "F", "G", "H"),
							  array("I", "J", "K", "L"), array("M", "N", "O", "P")));

$smarty->assign("contacts", array(array("phone" => "1", "fax" => "2", "cell" => "3"),
								  array("phone" => "555-4444", "fax" => "555-3333", "cell" => "760-1234")));

$smarty->display('index.tpl');

function insert_foo($args)
{
	extract($args);

	return "test $arg1";
}

?>
