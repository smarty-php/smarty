<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     html_image
 * Version:  1.0
 * Date:     Feb 24, 2003
 * Author:	 Monte Ohrt <monte@ispi.net>
 * Purpose:  format HTML tags for the image
 * Input:    name = name of image (required)
 *           border = border width (optional, default 0)
 *           height = image height (optional, default actual height)
 *           image =image width (optional, default actual width)
 *           basedir= base directory for absolute paths, default
 *           is environment variable DOCUMENT_ROOT
 * 
 * Examples: {image name="images/masthead.gif"}
 * Output:   <img src="images/masthead.gif" border=0 width=400 height=23>
 * -------------------------------------------------------------
 */
function smarty_function_html_image($params, &$smarty)
{	
	$name = '';
	$border = 0;
	$height = null;
	$width = null;
	$basedir = isset($GLOBALS['HTTP_SERVER_VARS']['DOCUMENT_ROOT'])
			? $GLOBALS['HTTP_SERVER_VARS']['DOCUMENT_ROOT'] : null;
	
    extract($params);

    if (empty($name)) {
        $smarty->trigger_error("html_image: missing 'name' parameter", E_USER_ERROR);
    }

	if(substr($name,0,1) == DIR_SEP) {
		$_image_path = $basedir . DIR_SEP . $name;
	} else {
		$_image_path = $name;
	}
	
	if(!file_exists($_image_path)) {
        $smarty->trigger_error("html_image: unable to find '$_image_path'", E_USER_ERROR);		
	}

	if(!is_readable($_image_path)) {
        $smarty->trigger_error("html_image: unable to read '$_image_path'", E_USER_ERROR);		
	}
	
	if(!$_image_data = getimagesize($_image_path)) {
        $smarty->trigger_error("html_image: '$_image_path' is not a valid image file", E_USER_ERROR);
	}
			
	return "<img src=\"$name\" border=\"$border\" ".$_image_data[3].'>';
}

/* vim: set expandtab: */

?>
