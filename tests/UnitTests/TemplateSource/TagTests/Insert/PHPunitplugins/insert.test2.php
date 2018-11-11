<?php

function smarty_insert_test2($params, $smarty) {

	return $smarty->tpl_vars[$params['var']]->value;
}
