<?php

namespace Smarty\Extension;

use Smarty\Compile\Tag\BCPluginWrapper;

class BCPluginsAdapter extends Base {

	/**
	 * @var \Smarty\Smarty
	 */
	private $smarty;

	public function __construct(\Smarty\Smarty $smarty) {
		$this->smarty = $smarty;
	}

	//$smarty->registered_plugins[$type][$name] = [$callback, (bool)$cacheable, (array)$cache_attr];
	public function getTagCompiler(string $tag): ?\Smarty\Compile\Tag\TagCompilerInterface {
		if (!isset($smarty->registered_plugins['compiler'][$tag])) {
			return null;
		}

		$callback = reset($smarty->registered_plugins['compiler'][$tag]);
		return new BCPluginWrapper($callback);
	}
}