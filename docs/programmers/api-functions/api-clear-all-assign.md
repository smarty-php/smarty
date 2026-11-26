clearAllAssign()

clears the values of all assigned variables

Description
===========

void

clearAllAssign


    <?php
    // passing name/value pairs
    $smarty->assign('Name', 'Fred');
    $smarty->assign('Address', $address);

    // will output above
    print_r( $smarty->getTemplateVars() );

    // clear all assigned variables
    $smarty->clearAllAssign();

    // will output nothing
    print_r( $smarty->getTemplateVars() );

    ?>

       

See also [`clearAssign()`](../../programmers/api-functions/api-clear-assign.md),
[`clearConfig()`](../../programmers/api-functions/api-clear-config.md),
[`getTemplateVars()`](../../programmers/api-functions/api-get-template-vars.md), 
[`assign()`](../../programmers/api-functions/api-assign.md)
and [`append()`](../../programmers/api-functions/api-append.md)
