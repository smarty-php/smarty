<?php 
/*
  +----------------------------------------------------------------------+
  | PHP Version 5                                                        |
  +----------------------------------------------------------------------+
  | Copyright (c) 1997-2004 The PHP Group                                |
  +----------------------------------------------------------------------+
  | This source file is subject to version 3.0 of the PHP license,       |
  | that is bundled with this package in the file LICENSE, and is        |
  | available through the world-wide-web at the following url:           |
  | http://www.php.net/license/3_0.txt.                                  |
  | If you did not receive a copy of the PHP license and are unable to   |
  | obtain it through the world-wide-web, please send a note to          |
  | license@php.net so we can mail you a copy immediately.               |
  +----------------------------------------------------------------------+
  | Authors:    Nuno Lopes <nlopess@php.net>                             |
  +----------------------------------------------------------------------+
  | Small hack to generate the manual for the web                        |
  +----------------------------------------------------------------------+

  $Id$
*/

set_time_limit(0);

$search = array(
    '<BODY',
    '</BODY'
);

$replace = array(
    '<?php if(!isset($_GET["print"]) && !strstr($_SERVER["REQUEST_URI"],"/print/")) { commonHeader(); } ?><BODY',
    '<?php if(!isset($_GET["print"]) && !strstr($_SERVER["REQUEST_URI"],"/print/")) { commonFooter(); } ?></BODY'
);

if ($dir = opendir('phpweb')) {
    echo "Processing the manual...\n";

    while (false !== ($file = readdir($dir))) {
        if(substr($file, -4) == '.php') {

            $text = file_get_contents('phpweb/' . $file);
            $text = str_replace($search, $replace, $text);

            $handler = fopen('phpweb/' . $file, 'w+');
            fputs($handler, $text);
            fclose($handler);
        }
    }

   closedir($dir); 
} else {
    die('Could not open the specified dir!');
}

?>
