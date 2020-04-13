<?php
function smarty_insert_insertplugintest($params, $template)
{
    global $insertglobal;

    return 'param foo ' . $params['foo'] . ' globalvar ' . $insertglobal;
}
