<?php
/**
 * Smarty Internal Plugin Smarty Template  Base
 * This file contains the basic shared methods for template handling
 *
 * @package    Smarty
 * @subpackage Template
 * @author     Uwe Tews
 */

use Smarty\Cacheresource\Base;
use Smarty\Data;
use Smarty\Smarty;

/**
 * Class with shared smarty/template methods
 *
 * @property int $_objType
 *
 */
abstract class Smarty_Internal_TemplateBase extends Data
{
    /**
     * Set this if you want different sets of cache files for the same
     * templates.
     *
     * @var string
     */
    public $cache_id = null;

    /**
     * Set this if you want different sets of compiled files for the same
     * templates.
     *
     * @var string
     */
    public $compile_id = null;

    /**
     * caching enabled
     *
     * @var int
     */
    public $caching = \Smarty\Smarty::CACHING_OFF;

    /**
     * check template for modifications?
     *
     * @var int
     */
    public $compile_check = \Smarty\Smarty::COMPILECHECK_ON;

    /**
     * cache lifetime in seconds
     *
     * @var integer
     */
    public $cache_lifetime = 3600;

    /**
     * Array of source information for known template functions
     *
     * @var array
     */
    public $tplFunctions = array();

    /**
     * When initialized to an (empty) array, this variable will hold a stack of template variables.
     *
     * @var null|array
     */
    public $_var_stack = null;

	/**
	 * Valid filter types
	 *
	 * @var array
	 */
	private $filterTypes = array('pre' => true, 'post' => true, 'output' => true, 'variable' => true);


	/**
     * fetches a rendered Smarty template
     *
     * @param string $template   the resource handle of the template file or template object
     * @param mixed  $cache_id   cache id to be used with this template
     * @param mixed  $compile_id compile id to be used with this template
     * @param object $parent     next higher level of Smarty variables
     *
     * @throws Exception
     * @throws SmartyException
     * @return string rendered template output
     */
    public function fetch($template = null, $cache_id = null, $compile_id = null, $parent = null)
    {
        $result = $this->_execute($template, $cache_id, $compile_id, $parent, 0);
        return $result === null ? ob_get_clean() : $result;
    }

    /**
     * displays a Smarty template
     *
     * @param string $template   the resource handle of the template file or template object
     * @param mixed  $cache_id   cache id to be used with this template
     * @param mixed  $compile_id compile id to be used with this template
     * @param object $parent     next higher level of Smarty variables
     *
     * @throws \Exception
     * @throws \SmartyException
     */
    public function display($template = null, $cache_id = null, $compile_id = null, $parent = null)
    {
        // display template
        $this->_execute($template, $cache_id, $compile_id, $parent, 1);
    }

    /**
     * test if cache is valid
     *
     * @api  Smarty::isCached()
     * @link https://www.smarty.net/docs/en/api.is.cached.tpl
     *
     * @param null|string|\Smarty_Internal_Template $template   the resource handle of the template file or template
     *                                                          object
     * @param mixed                                 $cache_id   cache id to be used with this template
     * @param mixed                                 $compile_id compile id to be used with this template
     * @param object                                $parent     next higher level of Smarty variables
     *
     * @return bool cache status
     * @throws \Exception
     * @throws \SmartyException
     */
    public function isCached($template = null, $cache_id = null, $compile_id = null, $parent = null)
    {
        return $this->_execute($template, $cache_id, $compile_id, $parent, 2);
    }

    /**
     * fetches a rendered Smarty template
     *
     * @param string $template   the resource handle of the template file or template object
     * @param mixed  $cache_id   cache id to be used with this template
     * @param mixed  $compile_id compile id to be used with this template
     * @param object $parent     next higher level of Smarty variables
     * @param string $function   function type 0 = fetch,  1 = display, 2 = isCache
     *
     * @return mixed
     * @throws \Exception
     * @throws \SmartyException
     */
    private function _execute($template, $cache_id, $compile_id, $parent, $function)
    {
        $smarty = $this->_getSmartyObj();
        $saveVars = true;
        if ($template === null) {
            if (!$this->_isTplObj()) {
                throw new SmartyException($function . '():Missing \'$template\' parameter');
            } else {
                $template = $this;
            }
        } elseif (is_object($template)) {
            /* @var Smarty_Internal_Template $template */
            if (!isset($template->_objType) || !$template->_isTplObj()) {
                throw new SmartyException($function . '():Template object expected');
            }
        } else {
            // get template object
            $saveVars = false;
            $template = $smarty->createTemplate($template, $cache_id, $compile_id, $parent ? $parent : $this, false);
            if ($this->_objType === 1) {
                // set caching in template object
                $template->caching = $this->caching;
            }
        }
        // make sure we have integer values
        $template->caching = (int)$template->caching;
        // fetch template content
        $level = ob_get_level();
        try {
            $_smarty_old_error_level =
                isset($smarty->error_reporting) ? error_reporting($smarty->error_reporting) : null;

            if ($smarty->isMutingUndefinedOrNullWarnings()) {
                $errorHandler = new \Smarty\ErrorHandler();
                $errorHandler->activate();
            }

            if ($this->_objType === 2) {
                /* @var Smarty_Internal_Template $this */
                $template->tplFunctions = $this->tplFunctions;
                $template->inheritance = $this->inheritance;
            }
            /* @var Smarty_Internal_Template $parent */
            if (isset($parent->_objType) && ($parent->_objType === 2) && !empty($parent->tplFunctions)) {
                $template->tplFunctions = array_merge($parent->tplFunctions, $template->tplFunctions);
            }
            if ($function === 2) {
                if ($template->caching) {
                    // return cache status of template
                    if (!isset($template->cached)) {
                        $template->loadCached();
                    }
                    $result = $template->cached->isCached($template);
                    Smarty_Internal_Template::$isCacheTplObj[ $template->_getTemplateId() ] = $template;
                } else {
                    return false;
                }
            } else {
                if ($saveVars) {
                    $savedTplVars = $template->tpl_vars;
                    $savedConfigVars = $template->config_vars;
                }
                ob_start();
                $template->_mergeVars();
                if (!empty(\Smarty\Smarty::$global_tpl_vars)) {
                    $template->tpl_vars = array_merge(\Smarty\Smarty::$global_tpl_vars, $template->tpl_vars);
                }
                $result = $template->render(false, $function);
                $template->_cleanUp();
                if ($saveVars) {
                    $template->tpl_vars = $savedTplVars;
                    $template->config_vars = $savedConfigVars;
                } else {
                    if (!$function && !isset(Smarty_Internal_Template::$tplObjCache[ $template->templateId ])) {
                        $template->parent = null;
                        $template->tpl_vars = $template->config_vars = array();
                        Smarty_Internal_Template::$tplObjCache[ $template->templateId ] = $template;
                    }
                }
            }

            if (isset($errorHandler)) {
                $errorHandler->deactivate();
            }

            if (isset($_smarty_old_error_level)) {
                error_reporting($_smarty_old_error_level);
            }
            return $result;
        } catch (Throwable $e) {
            while (ob_get_level() > $level) {
                ob_end_clean();
            }
            if (isset($errorHandler)) {
                $errorHandler->deactivate();
            }

            if (isset($_smarty_old_error_level)) {
                error_reporting($_smarty_old_error_level);
            }
            throw $e;
        }
    }

    /**
     * Registers plugin to be used in templates
     *
     * @api  Smarty::registerPlugin()
     * @link https://www.smarty.net/docs/en/api.register.plugin.tpl
     *
     * @param string   $type       plugin type
     * @param string   $name       name of template tag
     * @param callable $callback   PHP callback to register
     * @param bool     $cacheable  if true (default) this function is cache able
     * @param mixed    $cache_attr caching attributes if any
     *
     * @return \Smarty|\Smarty_Internal_Template
     * @throws \SmartyException
     */
    public function registerPlugin($type, $name, $callback, $cacheable = true, $cache_attr = null)
    {
	    $smarty = $this->_getSmartyObj();
	    if (isset($smarty->registered_plugins[ $type ][ $name ])) {
		    throw new SmartyException("Plugin tag '{$name}' already registered");
	    } elseif (!is_callable($callback)) {
		    throw new SmartyException("Plugin '{$name}' not callable");
	    } elseif ($cacheable && $cache_attr) {
		    throw new SmartyException("Cannot set caching attributes for plugin '{$name}' when it is cacheable.");
	    } else {
		    $smarty->registered_plugins[ $type ][ $name ] = array($callback, (bool)$cacheable, (array)$cache_attr);
	    }
	    return $this;
    }

	/**
	 * Registers plugin to be used in templates
	 *
	 * @api  Smarty::unregisterPlugin()
	 * @link https://www.smarty.net/docs/en/api.unregister.plugin.tpl
	 *
	 * @param string                                                          $type plugin type
	 * @param string                                                          $name name of template tag
	 *
	 * @return \Smarty|\Smarty_Internal_Template
	 */
	public function unregisterPlugin($type, $name)
	{
		$smarty = $this->_getSmartyObj();
		if (isset($smarty->registered_plugins[ $type ][ $name ])) {
			unset($smarty->registered_plugins[ $type ][ $name ]);
		}
		return $this;
	}

    /**
     * load a filter of specified type and name
     *
     * @api  Smarty::loadFilter()
     * @link https://www.smarty.net/docs/en/api.load.filter.tpl
     *
     * @param string $type filter type
     * @param string $name filter name
     *
     * @return bool
     * @throws \SmartyException
     */
    public function loadFilter($type, $name)
    {
	    $smarty = $this->_getSmartyObj();
	    $this->_checkFilterType($type);
	    $_plugin = "smarty_{$type}filter_{$name}";
	    $_filter_name = $_plugin;
	    if (is_callable($_plugin)) {
		    $smarty->registered_filters[ $type ][ $_filter_name ] = $_plugin;
		    return true;
	    }
	    if (class_exists($_plugin, false)) {
		    $_plugin = array($_plugin, 'execute');
	    }
	    if (is_callable($_plugin)) {
		    $smarty->registered_filters[ $type ][ $_filter_name ] = $_plugin;
		    return true;
	    }
	    throw new SmartyException("{$type}filter '{$name}' not found or callable");
    }

	/**
	 * load a filter of specified type and name
	 *
	 * @api  Smarty::unloadFilter()
	 *
	 * @link https://www.smarty.net/docs/en/api.unload.filter.tpl
	 *
	 * @param string                                                          $type filter type
	 * @param string                                                          $name filter name
	 *
	 * @return Smarty_Internal_TemplateBase
	 * @throws \SmartyException
	 */
	public function unloadFilter($type, $name)
	{
		$smarty = $this->_getSmartyObj();
		$this->_checkFilterType($type);
		if (isset($smarty->registered_filters[ $type ])) {
			$_filter_name = "smarty_{$type}filter_{$name}";
			if (isset($smarty->registered_filters[ $type ][ $_filter_name ])) {
				unset($smarty->registered_filters[ $type ][ $_filter_name ]);
				if (empty($smarty->registered_filters[ $type ])) {
					unset($smarty->registered_filters[ $type ]);
				}
			}
		}
		return $this;
	}

    /**
     * Registers a filter function
     *
     * @param string      $type filter type
     * @param callable    $callback
     * @param string|null $name optional filter name
     *
     * @return Smarty_Internal_TemplateBase
     * @throws \SmartyException
     *@link https://www.smarty.net/docs/en/api.register.filter.tpl
     *
     * @api  Smarty::registerFilter()
     */
    public function registerFilter($type, $callback, $name = null)
    {
	    $smarty = $this->_getSmartyObj();
	    $this->_checkFilterType($type);
	    $name = isset($name) ? $name : $this->_getFilterName($callback);
	    if (!is_callable($callback)) {
		    throw new SmartyException("{$type}filter '{$name}' not callable");
	    }
	    $smarty->registered_filters[ $type ][ $name ] = $callback;
	    return $this;
    }

	/**
	 * Unregisters a filter function
	 *
	 * @param string                                                          $type filter type
	 * @param callback|string                                                 $callback
	 *
	 * @return Smarty_Internal_TemplateBase
	 * @throws \SmartyException
	 *@api  Smarty::unregisterFilter()
	 *
	 * @link https://www.smarty.net/docs/en/api.unregister.filter.tpl
	 *
	 */
	public function unregisterFilter($type, $callback)
	{
		$smarty = $this->_getSmartyObj();
		$this->_checkFilterType($type);
		if (isset($smarty->registered_filters[ $type ])) {
			$name = is_string($callback) ? $callback : $this->_getFilterName($callback);
			if (isset($smarty->registered_filters[ $type ][ $name ])) {
				unset($smarty->registered_filters[ $type ][ $name ]);
				if (empty($smarty->registered_filters[ $type ])) {
					unset($smarty->registered_filters[ $type ]);
				}
			}
		}
		return $this;
	}

    /**
     * Registers object to be used in templates
     *
     * @api  Smarty::registerObject()
     * @link https://www.smarty.net/docs/en/api.register.object.tpl
     *
     * @param string $object_name
     * @param object $object                     the referenced PHP object to register
     * @param array  $allowed_methods_properties list of allowed methods (empty = all)
     * @param bool   $format                     smarty argument format, else traditional
     * @param array  $block_methods              list of block-methods
     *
     * @return \Smarty|\Smarty_Internal_Template
     * @throws \SmartyException
     */
    public function registerObject(
        $object_name,
        $object,
        $allowed_methods_properties = array(),
        $format = true,
        $block_methods = array()
    ) {
	    $smarty = $this->_getSmartyObj();
	    // test if allowed methods callable
	    if (!empty($allowed_methods_properties)) {
		    foreach ((array)$allowed_methods_properties as $method) {
			    if (!is_callable(array($object, $method)) && !property_exists($object, $method)) {
				    throw new SmartyException("Undefined method or property '$method' in registered object");
			    }
		    }
	    }
	    // test if block methods callable
	    if (!empty($block_methods)) {
		    foreach ((array)$block_methods as $method) {
			    if (!is_callable(array($object, $method))) {
				    throw new SmartyException("Undefined method '$method' in registered object");
			    }
		    }
	    }
	    // register the object
	    $smarty->registered_objects[ $object_name ] =
		    array($object, (array)$allowed_methods_properties, (boolean)$format, (array)$block_methods);
	    return $this;
    }

	/**
	 * Registers plugin to be used in templates
	 *
	 * @param string                                                          $object_name name of object
	 *
	 * @return Smarty_Internal_TemplateBase
	 * @api  Smarty::unregisterObject()
	 * @link https://www.smarty.net/docs/en/api.unregister.object.tpl
	 *
	 */
	public function unregisterObject($object_name)
	{
		$smarty = $this->_getSmartyObj();
		if (isset($smarty->registered_objects[ $object_name ])) {
			unset($smarty->registered_objects[ $object_name ]);
		}
		return $this;
	}

    /**
     * @param int $compile_check
     */
    public function setCompileCheck($compile_check)
    {
        $this->compile_check = (int)$compile_check;
    }

    /**
     * @param int $caching
     */
    public function setCaching($caching)
    {
        $this->caching = (int)$caching;
    }

    /**
     * @param int $cache_lifetime
     */
    public function setCacheLifetime($cache_lifetime)
    {
        $this->cache_lifetime = $cache_lifetime;
    }

    /**
     * @param string $compile_id
     */
    public function setCompileId($compile_id)
    {
        $this->compile_id = $compile_id;
    }

    /**
     * @param string $cache_id
     */
    public function setCacheId($cache_id)
    {
        $this->cache_id = $cache_id;
    }

	/**
	 * Add default modifiers
	 *
	 * @api Smarty::addDefaultModifiers()
	 *
	 * @param array|string                                                    $modifiers modifier or list of modifiers
	 *                                                                                   to add
	 *
	 * @return \Smarty|\Smarty_Internal_Template
	 */
	public function addDefaultModifiers($modifiers)
	{
		$smarty = $this->_getSmartyObj();
		if (is_array($modifiers)) {
			$smarty->default_modifiers = array_merge($smarty->default_modifiers, $modifiers);
		} else {
			$smarty->default_modifiers[] = $modifiers;
		}
		return $this;
	}

	/**
	 * creates a data object
	 *
	 * @param Data|null $parent next higher level of Smarty
	 *                                                                                     variables
	 * @param null $name optional data block name
	 *
	 * @return Smarty_Data data object
	 * @throws SmartyException
	 * @api  Smarty::createData()
	 * @link https://www.smarty.net/docs/en/api.create.data.tpl
	 *
	 */
	public function createData(Data $parent = null, $name = null)
	{
		/* @var Smarty $smarty */
		$smarty = $this->_getSmartyObj();
		$dataObj = new Smarty_Data($parent, $smarty, $name);
		if ($smarty->debugging) {
			\Smarty\Debug::register_data($dataObj);
		}
		return $dataObj;
	}

	/**
	 * return name of debugging template
	 *
	 * @api Smarty::getDebugTemplate()
	 *
	 * @return string
	 */
	public function getDebugTemplate()
	{
		$smarty = $this->_getSmartyObj();
		return $smarty->debug_tpl;
	}

	/**
	 * Get default modifiers
	 *
	 * @api Smarty::getDefaultModifiers()
	 *
	 * @return array list of default modifiers
	 */
	public function getDefaultModifiers()
	{
		$smarty = $this->_getSmartyObj();
		return $smarty->default_modifiers;
	}

	/**
	 * return a reference to a registered object
	 *
	 * @api  Smarty::getRegisteredObject()
	 * @link https://www.smarty.net/docs/en/api.get.registered.object.tpl
	 *
	 * @param string                                                          $object_name object name
	 *
	 * @return object
	 * @throws \SmartyException if no such object is found
	 */
	public function getRegisteredObject($object_name)
	{
		$smarty = $this->_getSmartyObj();
		if (!isset($smarty->registered_objects[ $object_name ])) {
			throw new SmartyException("'$object_name' is not a registered object");
		}
		if (!is_object($smarty->registered_objects[ $object_name ][ 0 ])) {
			throw new SmartyException("registered '$object_name' is not an object");
		}
		return $smarty->registered_objects[ $object_name ][ 0 ];
	}


	/**
	 * Get literals
	 *
	 * @api Smarty::getLiterals()
	 *
	 * @return array list of literals
	 */
	public function getLiterals()
	{
		$smarty = $this->_getSmartyObj();
		return (array)$smarty->literals;
	}

	/**
	 * Add literals
	 *
	 * @param array|string                                                    $literals literal or list of literals
	 *                                                                                  to addto add
	 *
	 * @return Smarty_Internal_TemplateBase
	 * @throws \SmartyException
	 * @api Smarty::addLiterals()
	 *
	 */
	public function addLiterals($literals = null)
	{
		if (isset($literals)) {
			$this->_setLiterals($this->_getSmartyObj(), (array)$literals);
		}
		return $this;
	}

	/**
	 * Set literals
	 *
	 * @param array|string                                                    $literals literal or list of literals
	 *                                                                                  to setto set
	 *
	 * @return Smarty_Internal_TemplateBase
	 * @throws \SmartyException
	 * @api Smarty::setLiterals()
	 *
	 */
	public function setLiterals($literals = null)
	{
		$smarty = $this->_getSmartyObj();
		$smarty->literals = array();
		if (!empty($literals)) {
			$this->_setLiterals($smarty, (array)$literals);
		}
		return $this;
	}

	/**
	 * common setter for literals for easier handling of duplicates the
	 * Smarty::$literals array gets filled with identical key values
	 *
	 * @param Smarty $smarty
	 * @param array   $literals
	 *
	 * @throws \SmartyException
	 */
	private function _setLiterals(Smarty $smarty, $literals)
	{
		$literals = array_combine($literals, $literals);
		$error = isset($literals[ $smarty->left_delimiter ]) ? array($smarty->left_delimiter) : array();
		$error = isset($literals[ $smarty->right_delimiter ]) ? $error[] = $smarty->right_delimiter : $error;
		if (!empty($error)) {
			throw new SmartyException(
				'User defined literal(s) "' . $error .
				'" may not be identical with left or right delimiter'
			);
		}
		$smarty->literals = array_merge((array)$smarty->literals, (array)$literals);
	}

	/**
	 * Check if filter type is valid
	 *
	 * @param string $type
	 *
	 * @throws \SmartyException
	 */
	private function _checkFilterType($type)
	{
		if (!isset($this->filterTypes[ $type ])) {
			throw new SmartyException("Illegal filter type '{$type}'");
		}
	}

	/**
	 * Return internal filter name
	 *
	 * @param callback $function_name
	 *
	 * @return string   internal filter name
	 */
	private function _getFilterName($function_name)
	{
		if (is_array($function_name)) {
			$_class_name = (is_object($function_name[ 0 ]) ? get_class($function_name[ 0 ]) : $function_name[ 0 ]);
			return $_class_name . '_' . $function_name[ 1 ];
		} elseif (is_string($function_name)) {
			return $function_name;
		} else {
			return 'closure';
		}
	}

	/**
	 * Registers static classes to be used in templates
	 *
	 * @param string                                                          $class_name
	 * @param string                                                          $class_impl the referenced PHP class to
	 *                                                                                    register
	 *
	 * @return Smarty_Internal_TemplateBase
	 * @throws \SmartyException
	 *@api  Smarty::registerClass()
	 * @link https://www.smarty.net/docs/en/api.register.class.tpl
	 *
	 */
	public function registerClass($class_name, $class_impl)
	{
		$smarty = $this->_getSmartyObj();
		// test if exists
		if (!class_exists($class_impl)) {
			throw new SmartyException("Undefined class '$class_impl' in register template class");
		}
		// register the class
		$smarty->registered_classes[ $class_name ] = $class_impl;
		return $this;
	}

	/**
	 * Registers a resource to fetch a template
	 *
	 * @param string                                                          $name name of resource type
	 * @param Base                                          $resource_handler
	 *
	 * @return Smarty_Internal_TemplateBase
	 * @link https://www.smarty.net/docs/en/api.register.cacheresource.tpl
	 *
	 * @api  Smarty::registerCacheResource()
	 */
	public function registerCacheResource($name, Base $resource_handler) {
		$smarty = $this->_getSmartyObj();
		$smarty->registered_cache_resources[ $name ] = $resource_handler;
		return $this;
	}

	/**
	 * Unregisters a resource to fetch a template
	 *
	 * @api  Smarty::unregisterCacheResource()
	 * @link https://www.smarty.net/docs/en/api.unregister.cacheresource.tpl
	 *
	 * @param                                                                 $name
	 *
	 * @return \Smarty|\Smarty_Internal_Template
	 */
	public function unregisterCacheResource($name)
	{
		$smarty = $this->_getSmartyObj();
		if (isset($smarty->registered_cache_resources[ $name ])) {
			unset($smarty->registered_cache_resources[ $name ]);
		}
		return $this;
	}

	/**
	 * Register config default handler
	 *
	 * @param callable                                                        $callback class/method name
	 *
	 * @return Smarty_Internal_TemplateBase
	 * @throws SmartyException              if $callback is not callable
	 *@api Smarty::registerDefaultConfigHandler()
	 *
	 */
	public function registerDefaultConfigHandler($callback)
	{
		$smarty = $this->_getSmartyObj();
		if (is_callable($callback)) {
			$smarty->default_config_handler_func = $callback;
		} else {
			throw new SmartyException('Default config handler not callable');
		}
		return $this;
	}

	/**
	 * Registers a default plugin handler
	 *
	 * @param callable                                                        $callback class/method name
	 *
	 * @return Smarty_Internal_TemplateBase
	 * @throws SmartyException              if $callback is not callable
	 * @link https://www.smarty.net/docs/en/api.register.default.plugin.handler.tpl
	 *
	 * @api  Smarty::registerDefaultPluginHandler()
	 */
	public function registerDefaultPluginHandler($callback)
	{
		$smarty = $this->_getSmartyObj();
		if (is_callable($callback)) {
			$smarty->default_plugin_handler_func = $callback;
		} else {
			throw new SmartyException("Default plugin handler '$callback' not callable");
		}
		return $this;
	}

	/**
	 * Register template default handler
	 *
	 * @param callable                                                        $callback class/method name
	 *
	 * @return Smarty_Internal_TemplateBase
	 * @throws SmartyException              if $callback is not callable
	 * @api Smarty::registerDefaultTemplateHandler()
	 *
	 */
	public function registerDefaultTemplateHandler($callback)
	{
		$smarty = $this->_getSmartyObj();
		if (is_callable($callback)) {
			$smarty->default_template_handler_func = $callback;
		} else {
			throw new SmartyException('Default template handler not callable');
		}
		return $this;
	}

	/**
	 * Registers a resource to fetch a template
	 *
	 * @api  Smarty::registerResource()
	 * @link https://www.smarty.net/docs/en/api.register.resource.tpl
	 *
	 * @param string                                                          $name             name of resource type
	 * @param Smarty_Resource                                           $resource_handler instance of Smarty_Resource
	 *
	 * @return \Smarty|\Smarty_Internal_Template
	 */
	public function registerResource($name, Smarty_Resource $resource_handler)
	{
		$smarty = $this->_getSmartyObj();
		$smarty->registered_resources[ $name ] = $resource_handler;
		return $this;
	}

	/**
	 * Unregisters a resource to fetch a template
	 *
	 * @param string $type name of resource type
	 *
	 * @return Smarty_Internal_TemplateBase
	 * @api  Smarty::unregisterResource()
	 * @link https://www.smarty.net/docs/en/api.unregister.resource.tpl
	 *
	 */
	public function unregisterResource($type)
	{
		$smarty = $this->_getSmartyObj();
		if (isset($smarty->registered_resources[ $type ])) {
			unset($smarty->registered_resources[ $type ]);
		}
		return $this;
	}

	/**
	 * set the debug template
	 *
	 * @param string                                                          $tpl_name
	 *
	 * @return Smarty_Internal_TemplateBase
	 * @throws SmartyException if file is not readable
	 *@api Smarty::setDebugTemplate()
	 *
	 */
	public function setDebugTemplate($tpl_name)
	{
		$smarty = $this->_getSmartyObj();
		if (!is_readable($tpl_name)) {
			throw new SmartyException("Unknown file '{$tpl_name}'");
		}
		$smarty->debug_tpl = $tpl_name;
		return $this;
	}

	/**
	 * Set default modifiers
	 *
	 * @param array|string                                                    $modifiers modifier or list of modifiers
	 *                                                                                   to set
	 *
	 * @return Smarty_Internal_TemplateBase
	 * @api Smarty::setDefaultModifiers()
	 *
	 */
	public function setDefaultModifiers($modifiers)
	{
		$smarty = $this->_getSmartyObj();
		$smarty->default_modifiers = (array)$modifiers;
		return $this;
	}

}
