addPluginsDir()

add a directory to the list of directories where plugins are stored

Description
===========

Smarty

addPluginsDir

string\|array

plugins\_dir


    <?php

    // add directory where plugins are stored
    $smarty->addPluginsDir('./plugins_1');

    // add multiple directories where plugins are stored
    $smarty->setPluginsDir(array(
        './plugins_2',
        './plugins_3',
    ));

    // view the plugins dir chain
    var_dump($smarty->getPluginsDir());

    // chaining of method calls
    $smarty->setPluginsDir('./plugins')
           ->addPluginsDir('./plugins_1')
           ->addPluginsDir('./plugins_2');

    ?>

       

See also [`getPluginsDir()`](../../programmers/api-functions/api-get-plugins-dir.md) and
[`setPluginsDir()`](../../programmers/api-functions/api-set-plugins-dir.md)
