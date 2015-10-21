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
 * @property Smarty_Internal_Runtime_Subtemplate           $_subtemplate
 * @property Smarty_Internal_Runtime_Inline                $_inline
 * @property Smarty_Internal_Runtime_Tplfunc               $_tplfunc
 * @property Smarty_Internal_Runtime_Var                   $_var
 * @property Smarty_Internal_Runtime_Foreach               $_foreach
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
                // debug output
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
                if ($this->smarty->debugging == 2 and $display === false) {
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
