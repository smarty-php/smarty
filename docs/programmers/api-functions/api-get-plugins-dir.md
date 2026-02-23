getPluginsDir()

return the directory where plugins are stored

Description
===========

array

getPluginsDir


    <?php

    // set some plugins directories
    $smarty->setPluginsDir(array(
        './plugins',
        './plugins_2',
    ));

    // get all directories where plugins are stored
    $config_dir = $smarty->getPluginsDir();
    var_dump($config_dir); // array

    ?>

       

See also [`setPluginsDir()`](../../programmers/api-functions/api-set-plugins-dir.md),
[`addPluginsDir()`](../../programmers/api-functions/api-add-plugins-dir.md) and
[`$plugins_dir`](#variable.plugins.dir).
