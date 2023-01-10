<?php
/**
 * Smarty Plugin Data
 * This file contains the data object
 *


 * @author     Uwe Tews
 */

namespace Smarty;

/**
 * class for the Smarty data object
 * The Smarty data object will hold Smarty variables in the current scope
 *


 */
class DataObject extends Data {

	/**
	 * Smarty object
	 *
	 * @var Smarty
	 */
	public $smarty = null;

	/**
	 * create Smarty data object
	 *
	 * @param Smarty|array $_parent parent template
	 * @param Smarty|Template $smarty global smarty instance
	 * @param string $name optional data block name
	 *
	 * @throws Exception
	 */
	public function __construct($_parent = null, $smarty = null, $name = null) {

		$this->smarty = $smarty;
		if (is_object($_parent)) {
			// when object set up back pointer
			$this->parent = $_parent;
		} elseif (is_array($_parent)) {
			// set up variable values
			foreach ($_parent as $_key => $_val) {
				$this->assign($_key, $_val);
			}
		} elseif ($_parent !== null) {
			throw new Exception('Wrong type for template variables');
		}
	}
}
