testInstall()

checks Smarty installation

Description
===========

void

testInstall

This function verifies that all required working folders of the Smarty
installation can be accessed. It does output a corresponding protocol.


    <?php
    use Smarty\Smarty;
    $smarty  = new Smarty();
    $smarty->testInstall();
    ?>

       
