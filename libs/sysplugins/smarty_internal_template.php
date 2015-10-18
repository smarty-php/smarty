<?php
/**
 * Smarty Internal Plugin Template
 * This file contains the Smarty template engine
 *
 * @package    Smarty
 * @subpackage Template
 * @author     Uwe Tews
 */

/**
 * Main class with template data structures and methods
 *
 * @package    Smarty
 * @subpackage Template
 *
 * @property Smarty_Template_Source|Smarty_Template_Config $source
 * @property Smarty_Template_Compiled                      $compiled
 * @property Smarty_Template_Cached                        $cached
 * @property Smarty_Internal_Runtime_Inheritance           $_inheritance
 * @method bool mustCompile()
 */
class Smarty_Internal_Template extends Smarty_Internal_TemplateBase
{
    /**
     * This object type (Smarty = 1, template = 2, data = 4)
     *
     * @var int
     */
    public $_objType = 2;

    /**
     * Global smarty instance
     *
     * @var Smarty
     */
    public $smarty = null;

    /**
     * Source instance
     *
     * @var Smarty_Template_Source|Smarty_Template_Config
     */
    public $source = null;

    /**
     * Template resource
     *
     * @var string
     */
    public $template_resource = null;

    /**
     * flag if compiled template is invalid and must be (re)compiled
     *
     * @var bool
     */
    public $mustCompile = null;

    /**
     * Template Id
     *
     * @var null|string
     */
    public $templateId = null;

    /**
     * Known template functions
     *
     * @var array
     */
    public $tpl_function = array();

    /**
     * Create template data object
     * Some of the global Smarty settings copied to template scope
     * It load the required template resources and caching plugins
     *
     * @param string                                                  $template_resource template resource string
     * @param Smarty                                                  $smarty            Smarty instance
     * @param \Smarty_Internal_Template|\Smarty|\Smarty_Internal_Data $_parent           back pointer to parent object
     *                                                                                   with variables or null
     * @param mixed                                                   $_cache_id         cache   id or null
     * @param mixed                                                   $_compile_id       compile id or null
     * @param bool                                                    $_caching          use caching?
     * @param int                                                     $_cache_lifetime   cache life-time in seconds
     *
     * @throws \SmartyException
     */
    public function __construct($template_resource, Smarty $smarty, Smarty_Internal_Data $_parent = null,
                                $_cache_id = null, $_compile_id = null, $_caching = null, $_cache_lifetime = null)
    {
        $this->smarty = &$smarty;
        // Smarty parameter
        $this->cache_id = $_cache_id === null ? $this->smarty->cache_id : $_cache_id;
        $this->compile_id = $_compile_id === null ? $this->smarty->compile_id : $_compile_id;
        $this->caching = $_caching === null ? $this->smarty->caching : $_caching;
        if ($this->caching === true) {
            $this->caching = Smarty::CACHING_LIFETIME_CURRENT;
        }
        $this->cache_lifetime = $_cache_lifetime === null ? $this->smarty->cache_lifetime : $_cache_lifetime;
        $this->parent = $_parent;
        // Template resource
        $this->template_resource = $template_resource;
        $this->source = Smarty_Template_Source::load($this);
    }

    /**
     * render template
     *
     * @param  bool $merge_tpl_vars   if true parent template variables merged in to local scope
     * @param  bool $no_output_filter if true do not run output filter
     * @param  bool $display          true: display, false: fetch null: subtemplate
     *
     * @throws Exception
     * @throws SmartyException
     * @return string rendered template output
     */
    public function render($merge_tpl_vars = false, $no_output_filter = true, $display = null)
    {
        $parentIsTpl = isset($this->parent) && $this->parent->_objType == 2;
        if ($this->smarty->debugging) {
            $this->smarty->_debug->start_template($this, $display);
        }
        // checks if template exists
        if (!$this->source->exists) {
            if ($parentIsTpl) {
                $parent_resource = " in '{$this->parent->template_resource}'";
            } else {
                $parent_resource = '';
            }
            throw new SmartyException("Unable to load template {$this->source->type} '{$this->source->name}'{$parent_resource}");
        }
        $save_tpl_vars = null;
        $save_config_vars = null;
        // merge all variable scopes into template
        if ($merge_tpl_vars) {
            // save local variables
            $save_tpl_vars = $this->tpl_vars;
            $save_config_vars = $this->config_vars;
            $ptr_array = array($this);
            $ptr = $this;
            while (isset($ptr->parent)) {
                $ptr_array[] = $ptr = $ptr->parent;
            }
            $ptr_array = array_reverse($ptr_array);
            $parent_ptr = reset($ptr_array);
            $tpl_vars = $parent_ptr->tpl_vars;
            $config_vars = $parent_ptr->config_vars;
            while ($parent_ptr = next($ptr_array)) {
                if (!empty($parent_ptr->tpl_vars)) {
                    $tpl_vars = array_merge($tpl_vars, $parent_ptr->tpl_vars);
                }
                if (!empty($parent_ptr->config_vars)) {
                    $config_vars = array_merge($config_vars, $parent_ptr->config_vars);
                }
            }
            if (!empty(Smarty::$global_tpl_vars)) {
                $tpl_vars = array_merge(Smarty::$global_tpl_vars, $tpl_vars);
            }
            $this->tpl_vars = $tpl_vars;
            $this->config_vars = $config_vars;
        }
        // check URL debugging control
        if (!$this->smarty->debugging && $this->smarty->debugging_ctrl == 'URL') {
            $this->smarty->_debug->debugUrl($this);
        }
        // disable caching for evaluated code
        if ($this->source->handler->recompiled) {
            $this->caching = false;
        }
        // read from cache or render
        $isCacheTpl =
            $this->caching == Smarty::CACHING_LIFETIME_CURRENT || $this->caching == Smarty::CACHING_LIFETIME_SAVED;
        if ($isCacheTpl) {
            if (!isset($this->cached)) {
                $this->loadCached();
            }
            $this->cached->render($this, $no_output_filter);
        } elseif ($this->source->handler->uncompiled) {
            $this->source->render($this);
        } else {
            if (!isset($this->compiled)) {
                $this->loadCompiled();
            }
            $this->compiled->render($this);
        }

        // display or fetch
        if ($display) {
            if ($this->caching && $this->smarty->cache_modified_check) {
                $this->cached->cacheModifiedCheck($this, isset($content) ? $content : ob_get_clean());
            } else {
                if ((!$this->caching || $this->cached->has_nocache_code || $this->source->handler->recompiled) &&
                    !$no_output_filter && (isset($this->smarty->autoload_filters['output']) ||
                        isset($this->smarty->registered_filters['output']))
                ) {
                    echo Smarty_Internal_Filter_Handler::runFilter('output', ob_get_clean(), $this);
                } else {
                    ob_end_flush();
                    flush();
                }
            }
            if ($this->smarty->debugging) {
                $this->smarty->_debug->end_template($this);
            }
            // debug output
            if ($this->smarty->debugging) {
                $this->smarty->_debug->display_debug($this, true);
            }
            if ($merge_tpl_vars) {
                // restore local variables
                $this->tpl_vars = $save_tpl_vars;
                $this->config_vars = $save_config_vars;
            }
            return '';
        } else {
            if ($merge_tpl_vars) {
                // restore local variables
                $this->tpl_vars = $save_tpl_vars;
                $this->config_vars = $save_config_vars;
            }
            if ($this->smarty->debugging) {
                $this->smarty->_debug->end_template($this);
            }
            if ($this->smarty->debugging == 2 and $display === false) {
                if ($this->smarty->debugging) {
                    $this->smarty->_debug->display_debug($this, true);
                }
            }
            if ($parentIsTpl) {
                if (!empty($this->tpl_function)) {
                    $this->parent->tpl_function = array_merge($this->parent->tpl_function, $this->tpl_function);
                }
                foreach ($this->compiled->required_plugins as $code => $tmp1) {
                    foreach ($tmp1 as $name => $tmp) {
                        foreach ($tmp as $type => $data) {
                            $this->parent->compiled->required_plugins[$code][$name][$type] = $data;
                        }
                    }
                }
            }
            if (!$no_output_filter &&
                (!$this->caching || $this->cached->has_nocache_code || $this->source->handler->recompiled) &&
                (isset($this->smarty->autoload_filters['output']) || isset($this->smarty->registered_filters['output']))
            ) {
                return Smarty_Internal_Filter_Handler::runFilter('output', ob_get_clean(), $this);
            }
            // return cache content
            return null;
        }
    }

    /**
     * Compiles the template
     * If the template is not evaluated the compiled template is saved on disk
     */
    public function compileTemplateSource()
    {
        return $this->compiled->compileTemplateSource($this);
    }

    /**
     * Writes the content to cache resource
     *
     * @param string $content
     *
     * @return bool
     */
    public function writeCachedContent($content)
    {
        return $this->cached->writeCachedContent($this, $content);
    }

    /**
     * Get unique template id
     *
     * @return string
     */
    public function _getTemplateId()
    {
        return isset($this->templateId) ? $this->templateId : $this->templateId =
            $this->smarty->_getTemplateId($this->template_resource, $this->cache_id, $this->compile_id);
    }

    /**
     * Template code runtime function to set up an inline subtemplate
     *
     * @param string      $template       template name
     * @param mixed       $cache_id       cache id
     * @param mixed       $compile_id     compile id
     * @param integer     $caching        cache mode
     * @param integer     $cache_lifetime life time of cache data
     * @param array       $data           passed parameter template variables
     * @param int         $parent_scope   scope in which {include} should execute
     * @param bool        $cache_tpl_obj  cache template object
     * @param string|null $uid            source uid
     *
     * @return \Smarty_Internal_Template template object
     * @throws \SmartyException
     */
    public function setupSubtemplate($template, $cache_id, $compile_id, $caching, $cache_lifetime, $data, $parent_scope,
                                     $cache_tpl_obj, $uid = null)
    {
        // if there are cached template objects calculate $templateID
        $_templateId = isset($this->smarty->_cache['template_objects']) ?
            $this->smarty->_getTemplateId($template, $cache_id, $compile_id) : null;
        // already in template cache?
        /* @var Smarty_Internal_Template $tpl */
        if (isset($this->smarty->_cache['template_objects'][$_templateId])) {
            // clone cached template object because of possible recursive call
            $tpl = clone $this->smarty->_cache['template_objects'][$_templateId];
            $tpl->parent = $this;
            // if $caching mode changed the compiled resource is invalid
            if ((bool) $tpl->caching !== (bool) $caching) {
                unset($tpl->compiled);
            }
            // get variables from calling scope
            if ($parent_scope == Smarty::SCOPE_LOCAL) {
                $tpl->tpl_vars = $this->tpl_vars;
                $tpl->config_vars = $this->config_vars;
            }
            $tpl->tpl_function = $this->tpl_function;
            // copy inheritance object?
            if (isset($this->_inheritance)) {
                $tpl->_inheritance = $this->_inheritance;
            } else {
                unset($tpl->_inheritance);
            }
        } else {
            $tpl = clone $this;
            $tpl->parent = $this;
            if (!isset($tpl->templateId) || $tpl->templateId !== $_templateId) {
                $tpl->templateId = $_templateId;
                $tpl->template_resource = $template;
                $tpl->cache_id = $cache_id;
                $tpl->compile_id = $compile_id;
                // $uid is set if template is inline
                if (isset($uid)) {
                    // inline templates have same compiled resource
                    $tpl->compiled = $this->compiled;
                    // if template is called multiple times set flag to to cache template objects
                    if (isset($tpl->compiled->includes[$template]) && $tpl->compiled->includes[$template] > 1) {
                        $cache_tpl_obj = true;
                    }
                    if (isset($tpl->compiled->file_dependency[$uid])) {
                        list($filepath, $timestamp, $resource) = $tpl->compiled->file_dependency[$uid];
                        $tpl->source =
                            new Smarty_Template_Source(isset($tpl->smarty->_cache['resource_handlers'][$resource]) ?
                                                           $tpl->smarty->_cache['resource_handlers'][$resource] :
                                                           Smarty_Resource::load($tpl->smarty, $resource), $tpl->smarty,
                                                       $filepath, $resource, $filepath);
                        $tpl->source->filepath = $filepath;
                        $tpl->source->timestamp = $timestamp;
                        $tpl->source->exist = true;
                        $tpl->source->uid = $uid;
                    } else {
                        $tpl->source = null;
                    }
                } else {
                    $tpl->source = null;
                    unset($tpl->compiled);
                }
                if (!isset($tpl->source)) {
                    $tpl->source = Smarty_Template_Source::load($tpl);
                }
                unset($tpl->cached);
                // check if template object should be cached
                if (!$tpl->source->handler->recompiled && (isset($tpl->parent->templateId) &&
                        isset($tpl->smarty->_cache['template_objects'][$tpl->parent->templateId]) ||
                        ($cache_tpl_obj && $tpl->smarty->resource_cache_mode & Smarty::RESOURCE_CACHE_AUTOMATIC) ||
                        $tpl->smarty->resource_cache_mode & Smarty::RESOURCE_CACHE_ON)
                ) {
                    $tpl->smarty->_cache['template_objects'][$tpl->_getTemplateId()] = $tpl;
                }
            }
        }
        $tpl->caching = $caching;
        $tpl->cache_lifetime = $cache_lifetime;
        if ($caching == 9999) {
            $tpl->cached = $this->cached;
        }
        // get variables from calling scope
        if ($parent_scope != Smarty::SCOPE_LOCAL) {
            if ($parent_scope == Smarty::SCOPE_PARENT) {
                $tpl->tpl_vars = &$this->tpl_vars;
                $tpl->config_vars = &$this->config_vars;
            } elseif ($parent_scope == Smarty::SCOPE_GLOBAL) {
                $tpl->tpl_vars = &Smarty::$global_tpl_vars;
                $tpl->config_vars = $this->config_vars;
            } elseif ($parent_scope == Smarty::SCOPE_ROOT) {
                $ptr = $tpl->parent;
                while (!empty($ptr->parent)) {
                    $ptr = $ptr->parent;
                }
                $tpl->tpl_vars = &$ptr->tpl_vars;
                $tpl->config_vars = &$ptr->config_vars;
            } else {
                $tpl->tpl_vars = $this->tpl_vars;
                $tpl->config_vars = $this->config_vars;
            }
        }

        if (!empty($data)) {
            // set up variable values
            foreach ($data as $_key => $_val) {
                $tpl->tpl_vars[$_key] = new Smarty_Variable($_val);
            }
        }
        return $tpl;
    }

    /**
     * Template code runtime function to render inline subtemplate
     *
     * @param Smarty_Internal_Template $tpl
     * @param string                   $content_func name of content function
     *
     * @throws \Exception
     */
    public function renderInline(Smarty_Internal_Template $tpl, $content_func)
    {
        if ($this->smarty->debugging) {
            $this->smarty->_debug->start_template($tpl);
            $this->smarty->_debug->start_render($tpl);
        }
        $tpl->compiled->getRenderedTemplateCode($tpl, $content_func);
        if ($this->smarty->debugging) {
            $this->smarty->_debug->end_template($tpl);
            $this->smarty->_debug->end_render($tpl);
        }
        if ($tpl->caching == 9999 && $tpl->compiled->has_nocache_code) {
            $this->cached->hashes[$tpl->compiled->nocache_hash] = true;
        }
    }

    /**
     * Call template function
     *
     * @param string                           $name        template function name
     * @param object|\Smarty_Internal_Template $_smarty_tpl template object
     * @param array                            $params      parameter array
     * @param bool                             $nocache     true if called nocache
     *
     * @throws \SmartyException
     */
    public function callTemplateFunction($name, Smarty_Internal_Template $_smarty_tpl, $params, $nocache)
    {
        if (isset($_smarty_tpl->tpl_function[$name])) {
            if (!$_smarty_tpl->caching || ($_smarty_tpl->caching && $nocache)) {
                $function = $_smarty_tpl->tpl_function[$name]['call_name'];
            } else {
                if (isset($_smarty_tpl->tpl_function[$name]['call_name_caching'])) {
                    $function = $_smarty_tpl->tpl_function[$name]['call_name_caching'];
                } else {
                    $function = $_smarty_tpl->tpl_function[$name]['call_name'];
                }
            }
            if (function_exists($function)) {
                $function ($_smarty_tpl, $params);
                return;
            }
            // try to load template function dynamically
            if (Smarty_Internal_Function_Call_Handler::call($name, $_smarty_tpl, $function)) {
                $function ($_smarty_tpl, $params);
                return;
            }
        }
        throw new SmartyException("Unable to find template function '{$name}'");
    }

    /**
     * This function is executed automatically when a compiled or cached template file is included
     * - Decode saved properties from compiled template and cache files
     * - Check if compiled or cache file is valid
     *
     * @param  array $properties special template properties
     * @param  bool  $cache      flag if called from cache file
     *
     * @return bool  flag if compiled or cache file is valid
     */
    public function decodeProperties($properties, $cache = false)
    {
        $is_valid = true;
        if (Smarty::SMARTY_VERSION != $properties['version']) {
            // new version must rebuild
            $is_valid = false;
        } elseif (!empty($properties['file_dependency']) &&
            ((!$cache && $this->smarty->compile_check) || $this->smarty->compile_check == 1)
        ) {
            // check file dependencies at compiled code
            foreach ($properties['file_dependency'] as $_file_to_check) {
                if ($_file_to_check[2] == 'file' || $_file_to_check[2] == 'extends' || $_file_to_check[2] == 'php') {
                    if ($this->source->filepath == $_file_to_check[0]) {
                        // do not recheck current template
                        continue;
                        //$mtime = $this->source->getTimeStamp();
                    } else {
                        // file and php types can be checked without loading the respective resource handlers
                        $mtime = is_file($_file_to_check[0]) ? filemtime($_file_to_check[0]) : false;
                    }
                } elseif ($_file_to_check[2] == 'string') {
                    continue;
                } else {
                    $source = Smarty_Template_Source::load(null, $this->smarty, $_file_to_check[0]);
                    $mtime = $source->getTimeStamp();
                }
                if (!$mtime || $mtime > $_file_to_check[1]) {
                    $is_valid = false;
                    break;
                }
            }
        }
        if ($cache) {
            // CACHING_LIFETIME_SAVED cache expiry has to be validated here since otherwise we'd define the unifunc
            if ($this->caching === Smarty::CACHING_LIFETIME_SAVED && $properties['cache_lifetime'] >= 0 &&
                (time() > ($this->cached->timestamp + $properties['cache_lifetime']))
            ) {
                $is_valid = false;
            }
            $this->cached->cache_lifetime = $properties['cache_lifetime'];
            $this->cached->valid = $is_valid;
            $resource = $this->cached;
        } else {
            $this->mustCompile = !$is_valid;
            $resource = $this->compiled;
            $resource->includes = isset($properties['includes']) ? $properties['includes'] : array();
        }
        if ($is_valid) {
            $resource->unifunc = $properties['unifunc'];
            $resource->has_nocache_code = $properties['has_nocache_code'];
            //            $this->compiled->nocache_hash = $properties['nocache_hash'];
            $resource->file_dependency = $properties['file_dependency'];
            if (isset($properties['tpl_function'])) {
                $this->tpl_function = $properties['tpl_function'];
            }
        }
        return $is_valid;
    }

    /**
     * Template code runtime function to create a local Smarty variable for array assignments
     *
     * @param string $tpl_var template variable name
     * @param bool   $nocache cache mode of variable
     * @param int    $scope   scope of variable
     */
    public function createLocalArrayVariable($tpl_var, $nocache = false, $scope = Smarty::SCOPE_LOCAL)
    {
        if (!isset($this->tpl_vars[$tpl_var])) {
            $this->tpl_vars[$tpl_var] = new Smarty_Variable(array(), $nocache, $scope);
        } else {
            $this->tpl_vars[$tpl_var] = clone $this->tpl_vars[$tpl_var];
            if ($scope != Smarty::SCOPE_LOCAL) {
                $this->tpl_vars[$tpl_var]->scope = $scope;
            }
            if (!(is_array($this->tpl_vars[$tpl_var]->value) ||
                $this->tpl_vars[$tpl_var]->value instanceof ArrayAccess)
            ) {
                settype($this->tpl_vars[$tpl_var]->value, 'array');
            }
        }
    }

    /**
     * [util function] counts an array, arrayAccess/traversable or PDOStatement object
     *
     * @param  mixed $value
     *
     * @return int   the count for arrays and objects that implement countable, 1 for other objects that don't, and 0
     *               for empty elements
     */
    public function _count($value)
    {
        if (is_array($value) === true || $value instanceof Countable) {
            return count($value);
        } elseif ($value instanceof IteratorAggregate) {
            // Note: getIterator() returns a Traversable, not an Iterator
            // thus rewind() and valid() methods may not be present
            return iterator_count($value->getIterator());
        } elseif ($value instanceof Iterator) {
            return iterator_count($value);
        } elseif ($value instanceof PDOStatement) {
            return $value->rowCount();
        } elseif ($value instanceof Traversable) {
            return iterator_count($value);
        } elseif ($value instanceof ArrayAccess) {
            if ($value->offsetExists(0)) {
                return 1;
            }
        } elseif (is_object($value)) {
            return count($value);
        }
        return 0;
    }

    /**
     * runtime error not matching capture tags
     */
    public function capture_error()
    {
        throw new SmartyException("Not matching {capture} open/close in \"{$this->template_resource}\"");
    }

    /**
     * Load compiled object
     *
     */
    public function loadCompiled()
    {
        if (!isset($this->compiled)) {
            $this->compiled = Smarty_Template_Compiled::load($this);
        }
    }

    /**
     * Load cached object
     *
     */
    public function loadCached()
    {
        if (!isset($this->cached)) {
            $this->cached = Smarty_Template_Cached::load($this);
        }
    }

    /**
     * Load compiler object
     *
     * @throws \SmartyException
     */
    public function loadCompiler()
    {
        if (!class_exists($this->source->handler->compiler_class)) {
            $this->smarty->loadPlugin($this->source->handler->compiler_class);
        }
        $this->compiler = new $this->source->handler->compiler_class($this->source->handler->template_lexer_class,
                                                                     $this->source->handler->template_parser_class,
                                                                     $this->smarty);
    }

    /**
     * Handle unknown class methods
     *
     * @param string $name unknown method-name
     * @param array  $args argument array
     *
     * @return mixed
     * @throws SmartyException
     */
    public function __call($name, $args)
    {
        // method of Smarty object?
        if (method_exists($this->smarty, $name)) {
            return call_user_func_array(array($this->smarty, $name), $args);
        }
        // parent
        return parent::__call($name, $args);
    }

    /**
     * set Smarty property in template context
     *
     * @param string $property_name property name
     * @param mixed  $value         value
     *
     * @throws SmartyException
     */
    public function __set($property_name, $value)
    {
        if ($property_name[0] == '_') {
            $this->$property_name = $value;
            return;
        }
        switch ($property_name) {
            case 'compiled':
            case 'cached':
            case 'compiler':
                $this->$property_name = $value;
                return;
            default:
                // Smarty property ?
                if (property_exists($this->smarty, $property_name)) {
                    $this->smarty->$property_name = $value;
                    return;
                }
        }
        throw new SmartyException("invalid template property '$property_name'.");
    }

    /**
     * get Smarty property in template context
     *
     * @param string $property_name property name
     *
     * @return mixed|Smarty_Template_Cached
     * @throws SmartyException
     */
    public function __get($property_name)
    {
        // object properties of runtime template extensions will start with '_'
        if ($property_name[0] == '_') {
            $class = 'Smarty_Internal_Runtime' . $property_name;
            $class[24] = chr(ord($class[24]) & 0xDF);
            if (class_exists($class)) {
                return $this->$property_name = new $class();
            }
        }
        switch ($property_name) {
            case 'compiled':
                $this->loadCompiled();
                return $this->compiled;

            case 'cached':
                $this->loadCached();
                return $this->cached;

            case 'compiler':
                $this->loadCompiler();
                return $this->compiler;
            default:
                // Smarty property ?
                if (property_exists($this->smarty, $property_name)) {
                    return $this->smarty->$property_name;
                }
        }
        throw new SmartyException("template property '$property_name' does not exist.");
    }

    /**
     * Template data object destructor
     */
    public function __destruct()
    {
        if ($this->smarty->cache_locking && isset($this->cached) && $this->cached->is_locked) {
            $this->cached->handler->releaseLock($this->smarty, $this->cached);
        }
    }
}
