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
     * blocks for template inheritance
     *
     * @var array
     */
    public $block_data = array();

    /**
     * variable filters
     *
     * @var array
     */
    public $variable_filters = array();

    /**
     * optional log of tag/attributes
     *
     * @var array
     */
    public $used_tags = array();

    /**
     * internal flag to allow relative path in child template blocks
     *
     * @var bool
     */
    public $allow_relative_path = false;

    /**
     * internal capture runtime stack
     *
     * @var array
     */
    public $_capture_stack = array(0 => array());

    /**
     * Template Id
     *
     * @var string
     */
    public $templateId = '';

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
     * @param string                                                  $_templateId       optional from extern
     *
     * @throws \SmartyException
     */
    public function __construct($template_resource, Smarty $smarty, Smarty_Internal_Data $_parent = null, $_cache_id = null, $_compile_id = null, $_caching = null, $_cache_lifetime = null, $_templateId = null)
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
        $this->templateId = isset($_templateId) ? $_templateId : $this->getTemplateId($template_resource, $this->cache_id, $this->compile_id);
        // copy block data of template inheritance
        if ($this->parent instanceof Smarty_Internal_Template) {
            $this->block_data = $this->parent->block_data;
        }
    }

    /**
     * fetches rendered template
     *
     * @param  string $template   the resource handle of the template file or template object
     * @param  mixed  $cache_id   cache id to be used with this template
     * @param  mixed  $compile_id compile id to be used with this template
     * @param  object $parent     next higher level of Smarty variables
     *
     * @throws Exception
     * @throws SmartyException
     * @return string rendered template output
     */
    public function fetch($template = null, $cache_id = null, $compile_id = null, $parent = null)
    {
        return isset($template) ? $this->smarty->fetch($template, $cache_id, $compile_id, $parent) : $this->render(true, false, false);
    }

    /**
     * displays a Smarty template
     *
     * @param  string $template   the resource handle of the template file or template object
     * @param  mixed  $cache_id   cache id to be used with this template
     * @param  mixed  $compile_id compile id to be used with this template
     * @param  object $parent     next higher level of Smarty variables
     *
     * @return string
     */
    public function display($template = null, $cache_id = null, $compile_id = null, $parent = null)
    {
        return isset($template) ? $this->smarty->fetch($template, $cache_id, $compile_id, $parent, true) : $this->render(true, false, true);
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
        $parentIsTpl = $this->parent instanceof Smarty_Internal_Template;
        if ($this->smarty->debugging) {
            Smarty_Internal_Debug::start_template($this, $display);
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
        $_smarty_old_error_level = isset($this->smarty->error_reporting) ? error_reporting($this->smarty->error_reporting) : null;
        // check URL debugging control
        if (!$this->smarty->debugging && $this->smarty->debugging_ctrl == 'URL') {
            Smarty_Internal_Debug::debugUrl($this);
        }
        // disable caching for evaluated code
        if ($this->source->recompiled) {
            $this->caching = false;
        }
        // read from cache or render
        $isCacheTpl = $this->caching == Smarty::CACHING_LIFETIME_CURRENT ||
            $this->caching == Smarty::CACHING_LIFETIME_SAVED;
        if ($isCacheTpl) {
            if (!isset($this->cached)) {
                $this->loadCached();
            }
            $this->cached->isCached($this);
        }
        if (!($isCacheTpl) || !$this->cached->valid) {
            // render template (not loaded and not in cache)
            if ($this->smarty->debugging) {
                Smarty_Internal_Debug::start_render($this);
            }
            if (!$this->source->uncompiled) {
                // render compiled code
                if (!isset($this->compiled)) {
                    $this->loadCompiled();
                }
                $content = $this->compiled->render($this);
            } else {
                $content = $this->source->renderUncompiled($this);
            }
            if ($parentIsTpl && !empty($this->tpl_function)) {
                $this->parent->tpl_function = array_merge($this->parent->tpl_function, $this->tpl_function);
            }
            if ($this->smarty->debugging) {
                Smarty_Internal_Debug::end_render($this);
            }
            // write to cache when necessary
            if (!$this->source->recompiled && $isCacheTpl) {
                if ($this->smarty->debugging) {
                    Smarty_Internal_Debug::start_cache($this);
                }
                $this->cached->updateCache($this, $content, $no_output_filter);
                $compile_check = $this->smarty->compile_check;
                $this->smarty->compile_check = false;
                if ($parentIsTpl) {
                    $this->compiled->unifunc = $this->parent->compiled->unifunc;
                }
                if (!$this->cached->processed) {
                    $this->cached->process($this, true);
                }
                $this->smarty->compile_check = $compile_check;
                $content = $this->getRenderedTemplateCode($this->cached->unifunc);
                if ($this->smarty->debugging) {
                    Smarty_Internal_Debug::end_cache($this);
                }
            } else {
                if ($this->parent instanceof Smarty_Internal_Template && !empty($this->compiled->nocache_hash) &&
                    !empty($this->parent->compiled->nocache_hash)
                ) {
                    // replace nocache_hash
                    $content = str_replace("{$this->compiled->nocache_hash}", $this->parent->compiled->nocache_hash, $content);
                    $this->parent->compiled->has_nocache_code = $this->parent->compiled->has_nocache_code ||
                        $this->compiled->has_nocache_code;
                }
            }
        } else {
            if ($this->smarty->debugging) {
                Smarty_Internal_Debug::start_cache($this);
            }
            $content = $this->cached->render($this);
            if ($this->smarty->debugging) {
                Smarty_Internal_Debug::end_cache($this);
            }
        }
        if ((!$this->caching || $this->cached->has_nocache_code || $this->source->recompiled) && !$no_output_filter &&
            (isset($this->smarty->autoload_filters['output']) || isset($this->smarty->registered_filters['output']))
        ) {
            $content = Smarty_Internal_Filter_Handler::runFilter('output', $content, $this);
        }
        if (isset($_smarty_old_error_level)) {
            error_reporting($_smarty_old_error_level);
        }
        // display or fetch
        if ($display) {
            if ($this->caching && $this->smarty->cache_modified_check) {
                $this->cached->cacheModifiedCheck($this, $content);
            } else {
                echo $content;
            }
            if ($this->smarty->debugging) {
                Smarty_Internal_Debug::end_template($this);
            }
            // debug output
            if ($this->smarty->debugging) {
                Smarty_Internal_Debug::display_debug($this, true);
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
                Smarty_Internal_Debug::end_template($this);
            }
            if ($this->smarty->debugging == 2 and $display === false) {
                if ($this->smarty->debugging) {
                    Smarty_Internal_Debug::display_debug($this, true);
                }
            }
            if ($parentIsTpl) {
                $this->parent->tpl_function = array_merge($this->parent->tpl_function, $this->tpl_function);
                foreach ($this->compiled->required_plugins as $code => $tmp1) {
                    foreach ($tmp1 as $name => $tmp) {
                        foreach ($tmp as $type => $data) {
                            $this->parent->compiled->required_plugins[$code][$name][$type] = $data;
                        }
                    }
                }
            }
            // return cache content
            return $content;
        }
    }

    /**
     * get rendered template content by calling compiled or cached template code
     *
     * @param string $unifunc function with template code
     *
     * @return string
     * @throws \Exception
     */
    public function getRenderedTemplateCode($unifunc)
    {
        $level = ob_get_level();
        try {
            ob_start();
            if (empty($unifunc) || !is_callable($unifunc)) {
                throw new SmartyException("Invalid compiled template for '{$this->template_resource}'");
            }
            if (isset($this->smarty->security_policy)) {
                $this->smarty->security_policy->startTemplate($this);
            }
            array_unshift($this->_capture_stack, array());
            //
            // render compiled or saved template code
            //
            $unifunc($this);
            // any unclosed {capture} tags ?
            if (isset($this->_capture_stack[0][0])) {
                $this->capture_error();
            }
            array_shift($this->_capture_stack);
            if (isset($this->smarty->security_policy)) {
                $this->smarty->security_policy->exitTemplate();
            }
            return ob_get_clean();
        }
        catch (Exception $e) {
            while (ob_get_level() > $level) {
                ob_end_clean();
            }
            throw $e;
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
     * @param string     $template_name
     * @param null|mixed $cache_id
     * @param null|mixed $compile_id
     *
     * @return string
     */
    public function getTemplateId($template_name, $cache_id = null, $compile_id = null)
    {
        $cache_id = isset($cache_id) ? $cache_id : $this->cache_id;
        $compile_id = isset($compile_id) ? $compile_id : $this->compile_id;
        if ($this->smarty->allow_ambiguous_resources) {
            $_templateId = Smarty_Resource::getUniqueTemplateName($this, $template_name) . "#{$cache_id}#{$compile_id}";
        } else {
            $_templateId = $this->smarty->_joined_template_dir . "#{$template_name}#{$cache_id}#{$compile_id}";
        }
        if (isset($_templateId[150])) {
            $_templateId = sha1($_templateId);
        }
        return $_templateId;
    }

    /**
     * Template code runtime function to get subtemplate content
     *
     * @param string  $template       template name
     * @param mixed   $cache_id       cache id
     * @param mixed   $compile_id     compile id
     * @param integer $caching        cache mode
     * @param integer $cache_lifetime life time of cache data
     * @param array   $data           passed parameter template variables
     * @param int     $parent_scope   scope in which {include} should execute
     * @param bool    $cache_tpl_obj  cache template object
     *
     * @return string template content
     * @throws \SmartyException
     */
    public function getSubTemplate($template, $cache_id, $compile_id, $caching, $cache_lifetime, $data, $parent_scope, $cache_tpl_obj)
    {
        $tpl = $this->setupSubTemplate($template, $cache_id, $compile_id, $caching, $cache_lifetime, $data, $parent_scope, $cache_tpl_obj);
        return $tpl->render();
    }

    /**
     * Template code runtime function to set up an inline subtemplate
     *
     * @param string  $template       template name
     * @param mixed   $cache_id       cache id
     * @param mixed   $compile_id     compile id
     * @param integer $caching        cache mode
     * @param integer $cache_lifetime life time of cache data
     * @param array   $data           passed parameter template variables
     * @param int     $parent_scope   scope in which {include} should execute
     * @param bool    $cache_tpl_obj  cache template object
     *
     * @return \Smarty_Internal_Template template object
     */
    public function setupSubTemplate($template, $cache_id, $compile_id, $caching, $cache_lifetime, $data, $parent_scope, $cache_tpl_obj)
    {
        $_templateId = $this->getTemplateId($template, $cache_id, $compile_id);
        // already in template cache?
        if (isset($this->smarty->template_objects[$_templateId])) {
            // clone cached template object because of possible recursive call
            $tpl = clone $this->smarty->template_objects[$_templateId];
            $tpl->parent = $this;
            if ((bool) $tpl->caching !== (bool) $caching) {
                unset($tpl->compiled);
            }
            $tpl->caching = $caching;
            $tpl->cache_lifetime = $cache_lifetime;
        } else {
            $tpl = new $this->smarty->template_class($template, $this->smarty, $this, $cache_id, $compile_id, $caching, $cache_lifetime, $_templateId);
        }
        if (!$tpl->source->recompiled && !isset($this->smarty->template_objects[$_templateId]) &&
            (isset($this->smarty->template_objects[$this->templateId]) ||
                ($cache_tpl_obj && $this->smarty->resource_cache_mode & Smarty::RESOURCE_CACHE_AUTOMATIC) ||
                $this->smarty->resource_cache_mode & Smarty::RESOURCE_CACHE_ON)
        ) {
            $this->smarty->template_objects[$_templateId] = $tpl;
        }
        if ($caching == 9999) {
            $tpl->cached = $this->cached;
        }
        // get variables from calling scope
        if ($parent_scope == Smarty::SCOPE_LOCAL) {
            $tpl->tpl_vars = $this->tpl_vars;
        } elseif ($parent_scope == Smarty::SCOPE_PARENT) {
            $tpl->tpl_vars = &$this->tpl_vars;
        } elseif ($parent_scope == Smarty::SCOPE_GLOBAL) {
            $tpl->tpl_vars = &Smarty::$global_tpl_vars;
        } elseif (($scope_ptr = $this->getScopePointer($parent_scope)) == null) {
            $tpl->tpl_vars = &$this->tpl_vars;
        } else {
            $tpl->tpl_vars = &$scope_ptr->tpl_vars;
        }
        $tpl->config_vars = $this->config_vars;
        if (!empty($data)) {
            // set up variable values
            foreach ($data as $_key => $_val) {
                $tpl->tpl_vars[$_key] = new Smarty_Variable($_val);
            }
        }
        $tpl->tpl_function = $this->tpl_function;
        return $tpl;
    }

    /**
     * Template code runtime function to set up an inline subtemplate
     *
     * @param string  $template       template name
     * @param mixed   $cache_id       cache id
     * @param mixed   $compile_id     compile id
     * @param integer $caching        cache mode
     * @param integer $cache_lifetime life time of cache data
     * @param array   $data           passed parameter template variables
     * @param int     $parent_scope   scope in which {include} should execute
     * @param bool    $cache_tpl_obj  cache template object
     * @param string  $content_func   name of content function
     *
     * @return string template content
     * @throws \Exception
     */
    public function getInlineSubTemplate($template, $cache_id, $compile_id, $caching, $cache_lifetime, $data, $parent_scope, $cache_tpl_obj, $content_func)
    {
        $tpl = $this->setupSubTemplate($template, $cache_id, $compile_id, $caching, $cache_lifetime, $data, $parent_scope, $cache_tpl_obj);
        if (!isset($tpl->compiled)) {
            $tpl->compiled = $this->compiled;
        }
        if ($this->smarty->debugging) {
            Smarty_Internal_Debug::start_template($tpl);
            Smarty_Internal_Debug::start_render($tpl);
        }
        $output = $tpl->getRenderedTemplateCode($content_func);
        if ($this->smarty->debugging) {
            Smarty_Internal_Debug::end_template($tpl);
            Smarty_Internal_Debug::end_render($tpl);
        }
        return str_replace($tpl->compiled->nocache_hash, $this->compiled->nocache_hash, $output);
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
                if ($_file_to_check[2] == 'file' || $_file_to_check[2] == 'php') {
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
            $resource->includes = $properties['includes'];
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
     * Template code runtime function to get pointer to template variable array of requested scope
     *
     * @param  int $scope requested variable scope
     *
     * @return array array of template variables
     */
    public function &getScope($scope)
    {
        if ($scope == Smarty::SCOPE_PARENT && !empty($this->parent)) {
            return $this->parent->tpl_vars;
        } elseif ($scope == Smarty::SCOPE_ROOT && !empty($this->parent)) {
            $ptr = $this->parent;
            while (!empty($ptr->parent)) {
                $ptr = $ptr->parent;
            }

            return $ptr->tpl_vars;
        } elseif ($scope == Smarty::SCOPE_GLOBAL) {
            return Smarty::$global_tpl_vars;
        }
        $null = null;

        return $null;
    }

    /**
     * Get parent or root of template parent chain
     *
     * @param  int $scope parent or root scope
     *
     * @return mixed object
     */
    public function getScopePointer($scope)
    {
        if ($scope == Smarty::SCOPE_PARENT && !empty($this->parent)) {
            return $this->parent;
        } elseif ($scope == Smarty::SCOPE_ROOT && !empty($this->parent)) {
            $ptr = $this->parent;
            while (!empty($ptr->parent)) {
                $ptr = $ptr->parent;
            }

            return $ptr;
        }

        return null;
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
            if (!class_exists('Smarty_Template_Compiled', false)) {
                require SMARTY_SYSPLUGINS_DIR . 'smarty_template_compiled.php';
            }
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
            if (!class_exists('Smarty_Template_Cached', false)) {
                require SMARTY_SYSPLUGINS_DIR . 'smarty_template_cached.php';
            }
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
        if (!class_exists($this->source->compiler_class)) {
            $this->smarty->loadPlugin($this->source->compiler_class);
        }
        $this->compiler = new $this->source->compiler_class($this->source->template_lexer_class, $this->source->template_parser_class, $this->smarty);
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
