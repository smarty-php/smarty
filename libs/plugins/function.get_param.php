<?php
/**
 * Smarty plugin
 *
 * @package Smarty
 * @subpackage PluginsFunction
 * @author Jambik <> idea and creations
 *         alex Roosso <http://www.roocms.com> - mod: added flag noentitys
 */

/**
 * Smarty {get_params} function plugin
 *
 * Type:     function<br>
 * Name:     get_params<br>
 * Purpose:  generate get parameters
 * Input:
 *         - prefix  = before string
 *         - suffix  = after string
 *         - exclude = GET params wich should be excluded
 *
 * @author Jambik <> idea and creations
 *         alex Roosso <http://www.roocms.com> - mod: added flag noentitys
 * @param array                    $params   parameters
 * @param Smarty_Internal_Template $template template object
 * @return string
 */
function smarty_function_get_params($params, $template)
{
	$prefix = isset($params['prefix']) ? trim($params['prefix']) : "";
	$forcePrefix = isset($params['forcePrefix']) ? $params['forcePrefix'] : false;
	$suffix = isset($params['suffix']) ? trim($params['suffix']) : "";
	$excludeParams = isset($params['exclude']) ? trim($params['exclude']) : false;
	$noentity = isset($params['noentity']) ? true : false;
	$output = "";
	
	if($excludeParams) {
		$excludeParams = explode(",", $excludeParams);
		foreach($excludeParams as $key => $value) $excludeParams[$key] = trim($value);
	}
	
	if($_GET) {

		$entity = ($noentity) ? "&" : "&amp;" ;

		if($excludeParams) {
			foreach($_GET as $key=>$value) {
				if(!in_array($key, $excludeParams)) {
					$output .= $entity.$key."=".$value;
				}
			}
		}
		else {
			foreach($_GET as $key=>$value) {
				$output .= $entity.$key."=".$value;
			}
		}
		
		$output = $output ? substr($output, 5) : "";
	}
	
	$output = $output ? $prefix.$output.$suffix : ( $forcePrefix ? $prefix : "" );

	return $output;
}
