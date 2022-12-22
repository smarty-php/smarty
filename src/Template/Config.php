<?php

namespace Smarty\Template;

use Smarty;
use Smarty\Template;
use SmartyException;

/**
 * Smarty Config Resource Data Object
 * Meta Data Container for Config Files
 *
 * @author     Uwe Tews
 */
class Config extends Source {

	/**
	 * array of section names, single section or null
	 *
	 * @var null|string|array
	 */
	public $config_sections = null;

	/**
	 * scope into which the config variables shall be loaded
	 *
	 * @var int
	 */
	public $scope = 0;

	/**
	 * Flag that source is a config file
	 *
	 * @var bool
	 */
	public $isConfig = true;

	/**
	 * Name of the Class to compile this resource's contents with
	 *
	 * @var string
	 */
	public $compiler_class = 'Smarty\Compiler\ConfigFile';

	/**
	 * Name of the Class to tokenize this resource's contents with
	 *
	 * @var string
	 */
	public $template_lexer_class = 'Smarty_Internal_Configfilelexer';

	/**
	 * Name of the Class to parse this resource's contents with
	 *
	 * @var string
	 */
	public $template_parser_class = 'Smarty_Internal_Configfileparser';

	/**
	 * initialize Source Object for given resource
	 * Either [$_template] or [$smarty, $template_resource] must be specified
	 *
	 * @param Template $_template template object
	 * @param Smarty $smarty smarty object
	 * @param string $template_resource resource identifier
	 *
	 * @return Config Source Object
	 * @throws SmartyException
	 */
	public static function load(
		Template $_template = null,
		Smarty   $smarty = null,
		         $template_resource = null
	) {
		static $_incompatible_resources = ['extends' => true, 'php' => true];
		if ($_template) {
			$smarty = $_template->smarty;
			$template_resource = $_template->template_resource;
		}
		if (empty($template_resource)) {
			throw new SmartyException('Source: Missing  name');
		}
		// parse resource_name, load resource handler
		[$name, $type] = Smarty\Resource\BasePlugin::parseResourceName($template_resource, $smarty->default_config_type);
		// make sure configs are not loaded via anything smarty can't handle
		if (isset($_incompatible_resources[$type])) {
			throw new SmartyException("Unable to use resource '{$type}' for config");
		}
		$source = new Config($smarty, $template_resource, $type, $name);
		$source->handler->populate($source, $_template);
		if (!$source->exists && isset($smarty->default_config_handler_func)) {
			$source->_getDefaultTemplate($smarty->default_config_handler_func);
			$source->handler->populate($source, $_template);
		}
		return $source;
	}
}
