<?php
function smarty_function_chain1($params, $tpl)
{
    $tpl->smarty->loadPlugin('smarty_function_chain2');

    return smarty_function_chain2($params, $tpl);
}
