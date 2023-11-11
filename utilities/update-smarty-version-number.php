<?php

// This takes the Smarty class file and updates the SMARTY_VERSION constant

$path_to_smarty_class = 'src/Smarty.php';
$pattern = "/const SMARTY_VERSION = '[^']+'/";
$replacement = "const SMARTY_VERSION = '" . $argv[1] . "'";

$file_contents = preg_replace($pattern, $replacement, file_get_contents($path_to_smarty_class));

file_put_contents($path_to_smarty_class, $file_contents);
