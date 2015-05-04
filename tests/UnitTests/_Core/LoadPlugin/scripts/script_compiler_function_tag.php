<?php
function default_script_compiler_function_tag($params, $template)
{
    return "echo 'scriptcompilerfunction '." . $params['value'] . ";";
}
