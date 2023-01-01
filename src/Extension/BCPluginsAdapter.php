<?php

namespace Smarty\Extension;

use Smarty\BlockHandler\BlockPluginWrapper;
use Smarty\Compile\Modifier\BCPluginWrapper as ModifierCompilerPluginWrapper;
use Smarty\Compile\Tag\BCPluginWrapper as TagPluginWrapper;
use Smarty\FunctionHandler\BCPluginWrapper as FunctionPluginWrapper;

class BCPluginsAdapter extends Base {

	/**
	 * @var \Smarty\Smarty
	 */
	private $smarty;

	public function __construct(\Smarty\Smarty $smarty) {
		$this->smarty = $smarty;
	}

	private function findPlugin($type, $name): ?array {
		if (null !== $plugin = $this->smarty->getRegisteredPlugin($type, $name)) {
			return $plugin;
		}

		return null;
	}

	public function getTagCompiler(string $tag): ?\Smarty\Compile\Tag\TagCompilerInterface {

		$plugin = $this->findPlugin(\Smarty\Smarty::PLUGIN_COMPILER, $tag);
		if ($plugin === null) {
			return null;
		}

		$callback = $plugin[0];
		$cacheable = (bool) $plugin[1] ?? true;
		return new TagPluginWrapper($callback, $cacheable);
	}

	public function getFunctionHandler(string $functionName): ?\Smarty\FunctionHandler\FunctionHandlerInterface {
		$plugin = $this->findPlugin(\Smarty\Smarty::PLUGIN_FUNCTION, $functionName);
		if ($plugin === null) {
			return null;
		}
		$callback = $plugin[0];
		$cacheable = (bool) $plugin[1] ?? true;
		$cache_attributes = (array) $plugin[2] ?? [];

		return new FunctionPluginWrapper($callback, $cacheable, $cache_attributes);

	}

	public function getBlockHandler(string $blockTagName): ?\Smarty\BlockHandler\BlockHandlerInterface {
		$plugin = $this->findPlugin(\Smarty\Smarty::PLUGIN_BLOCK, $blockTagName);
		if ($plugin === null) {
			return null;
		}
		$callback = $plugin[0];

		return new BlockPluginWrapper($callback);
	}

	public function getModifierCallback(string $modifierName) {

		$plugin = $this->findPlugin(\Smarty\Smarty::PLUGIN_MODIFIER, $modifierName);
		if ($plugin === null) {
			return null;
		}
		return $plugin[0];
	}

	public function getModifierCompiler(string $modifier): ?\Smarty\Compile\Modifier\ModifierCompilerInterface {
		$plugin = $this->findPlugin(\Smarty\Smarty::PLUGIN_MODIFIERCOMPILER, $modifier);
		if ($plugin === null) {
			return null;
		}
		$callback = $plugin[0];

		return new ModifierCompilerPluginWrapper($callback);
	}

	public function loadPluginsFromDir(string $path) {

		foreach([
			'function',
			'modifier',
		    'block',
		    'compiler',
		    'prefilter',
		    'postfilter',
		    'outputfilter',
		    'resource',
		    'insert',
		] as $type) {
			foreach (glob($path  . $type . '.?*.php') as $filename) {
				$pluginName = $this->getPluginNameFromFilename($filename);
				if ($pluginName !== null) {
					require_once $filename;
					if (function_exists($functionName = 'smarty_' . $type . '_' . $pluginName)) {
						$this->smarty->registerPlugin($type, $pluginName, $functionName, true, []);
					}
				}
			}
		}

	}

	/**
	 * @param $filename
	 *
	 * @return string|null
	 */
	private function getPluginNameFromFilename($filename) {
		if (!preg_match('/.*\.([a-z_A-Z0-9]+)\.php$/',$filename,$matches)) {
			return null;
		}
		return $matches[1];
	}

}