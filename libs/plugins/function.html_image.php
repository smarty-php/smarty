<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * Type:     function
 * Name:     html_image
 * Version:  1.0
 * Date:     Feb 24, 2003
 * Author:	 Monte Ohrt <monte@ispi.net>
 * Credits:  Duda <duda@big.hu> - wrote first image function
 *           in repository, helped with lots of functionality
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
	require_once $smarty->_get_plugin_filepath('shared','escape_special_chars');

	$name = '';
	$border = 0;
	$height = '';
	$width = '';
	$extra = '';
	$prefix = '';
	$suffix = '';
	$basedir = isset($GLOBALS['HTTP_SERVER_VARS']['DOCUMENT_ROOT'])
			? $GLOBALS['HTTP_SERVER_VARS']['DOCUMENT_ROOT'] : '/';
	
	foreach($params as $_key => $_val) {	
		switch($_key) {
			case 'name':
				$name = $_val;
				break;
			case 'border':
				$border = $_val;
				break;
			case 'height':
				$height = $_val;
				break;
			case 'width':
				$width = $_val;
				break;
			case 'link':
				$prefix = '<a href="' . $link . '">';
				$suffix = '</a>';
				break;
			default:
				$extra .= ' '.$_key.'="'.smarty_function_escape_special_chars($_val).'"';
				break;					
		}
	}

    if (empty($name)) {
        $smarty->trigger_error("html_image: missing 'name' parameter", E_USER_ERROR);
    }

	if(substr($name,0,1) == DIR_SEP) {
		$_image_path = $basedir . DIR_SEP . $name;
	} else {
		$_image_path = $name;
	}
	
	if(!isset($params['width']) || !isset($params['height'])) {
		if(!$_image_data = getimagesize($_image_path)) {
			if(!file_exists($_image_path)) {
        		$smarty->trigger_error("html_image: unable to find '$_image_path'", E_USER_ERROR);		
			} else if(!is_readable($_image_path)) {
        		$smarty->trigger_error("html_image: unable to read '$_image_path'", E_USER_ERROR);		
			} else {
        		$smarty->trigger_error("html_image: '$_image_path' is not a valid image file", E_USER_ERROR);
			}
		}
		if(!$smarty->security && substr($_image_path,0,strlen($basedir)) != $basedir) {
        	$smarty->trigger_error("html_image: (secure) '$_image_path' not within basedir ($basedir)", E_USER_ERROR);		
		}	
		
		if(!isset($params['width'])) {
			$width = $_image_data[1];
		}
		if(!isset($params['height'])) {
			$height = $_image_data[2];
		}
		
	}
			
	return $prefix . '<img src="'.$name.'" border="'.$border.'" width="'.$width.'" height="'.$height.'"'.$extra.'>' . $suffix;
}

/* vim: set expandtab: */

?>
