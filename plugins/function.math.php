<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     math
 * Purpose:  handle math computations in template
 * -------------------------------------------------------------
 */
function smarty_function_math($args, &$smarty_obj)
{
    // be sure equation parameter is present
    if (empty($args["equation"])) {
        $smarty_obj->_trigger_error_msg("math: missing equation parameter");
        return;
    }

    $equation = $args["equation"];

    // make sure parenthesis are balanced
    if (substr_count($equation,"(") != substr_count($equation,")")) {
        $smarty_obj->_trigger_error_msg("math: unbalanced parenthesis");
        return;
    }

    // match all vars in equation, make sure all are passed
    preg_match_all("![a-zA-Z][a-zA-Z0-9]*!",$equation, $match);
    $allowed_funcs = array('int','abs','ceil','cos','exp','floor','log','log10',
                           'max','min','pi','pow','rand','round','sin','sqrt','srand','tan');

    foreach($match[0] as $curr_var) {
        if (!in_array($curr_var,array_keys($args)) && !in_array($curr_var, $allowed_funcs)) {
            $smarty_obj->_trigger_error_msg("math: parameter $curr_var not passed as argument");
            return;
        }
    }

    foreach($args as $key => $val) {
        if ($key != "equation" && $key != "format" && $key != "assign") {
            // make sure value is not empty
            if (strlen($val)==0) {
                $smarty_obj->_trigger_error_msg("math: parameter $key is empty");
                return;
            }
            if (!is_numeric($val)) {
                $smarty_obj->_trigger_error_msg("math: parameter $key: is not numeric");
                return;
            }
            $equation = preg_replace("/\b$key\b/",$val, $equation);
        }
    }

    eval("\$smarty_math_result = ".$equation.";");

    if (empty($args["format"])) {
        if (empty($args["assign"])) {
            echo $smarty_math_result;
        } else {
            $smarty_obj->assign($args["assign"],$smarty_math_result);
        }
    } else {
        if (empty($args["assign"])){
            printf($args["format"],$smarty_math_result);
        } else {
            $smarty_obj->assign($assign,sprintf($args["format"],$smarty_math_result));
        }
    }
}

/* vim: set expandtab: */

?>
