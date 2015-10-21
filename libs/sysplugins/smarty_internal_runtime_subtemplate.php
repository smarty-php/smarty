<?php

/**
 * Subtemplate Runtime Methods render, setupSubtemplate
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 *
 **/
class Smarty_Internal_Runtime_Subtemplate
{

    /**
     * Runtime function to render subtemplate
     *
     * @param \Smarty_Internal_Template $parent
     * @param string                    $template       template name
     * @param mixed                     $cache_id       cache id
     * @param mixed                     $compile_id     compile id
     * @param integer                   $caching        cache mode
     * @param integer                   $cache_lifetime life time of cache data
     * @param array                     $data           passed parameter template variables
     * @param int                       $parent_scope   scope in which {include} should execute
     * @param bool                      $cache_tpl_obj  cache template object
     *
     */
    public function render(Smarty_Internal_Template $parent, $template, $cache_id, $compile_id, $caching,
                           $cache_lifetime, $data, $parent_scope, $cache_tpl_obj)
    {
        $this->setupSubtemplate($parent, $template, $cache_id, $compile_id, $caching, $cache_lifetime, $data,
                                $parent_scope, $cache_tpl_obj)
             ->render();
    }

    /**
     * Runtime function to get subtemplate object from cache or clone from parent
     *
     * @param \Smarty_Internal_Template $parent         calling template
     * @param string                    $template       template name
     * @param mixed                     $cache_id       cache id
     * @param mixed                     $compile_id     compile id
     * @param integer                   $caching        cache mode
     * @param integer                   $cache_lifetime life time of cache data
     * @param array                     $data           passed parameter template variables
     * @param int                       $parent_scope   scope in which {include} should execute
     * @param bool                      $cache_tpl_obj  cache template object
     * @param string|null               $uid            source uid
     *
     * @return \Smarty_Internal_Template template object
     * @throws \SmartyException
     */
    public function setupSubtemplate(Smarty_Internal_Template $parent, $template, $cache_id, $compile_id, $caching,
                                     $cache_lifetime, $data, $parent_scope, $cache_tpl_obj, $uid = null)
    {
        // if there are cached template objects calculate $templateID
        $_templateId = isset($parent->smarty->_cache['template_objects']) ?
            $parent->smarty->_getTemplateId($template, $cache_id, $compile_id) : null;
        // already in template cache?
        /* @var Smarty_Internal_Template $tpl */
        if (isset($parent->smarty->_cache['template_objects'][$_templateId])) {
            // clone cached template object because of possible recursive call
            $tpl = clone $parent->smarty->_cache['template_objects'][$_templateId];
            $tpl->parent = $parent;
            // if $caching mode changed the compiled resource is invalid
            if ((bool) $tpl->caching !== (bool) $caching) {
                unset($tpl->compiled);
            }
            // get variables from calling scope
            if ($parent_scope == Smarty::SCOPE_LOCAL) {
                $tpl->tpl_vars = $parent->tpl_vars;
                $tpl->config_vars = $parent->config_vars;
            }
            $tpl->tpl_function = $parent->tpl_function;
            // copy inheritance object?
            if (isset($parent->_inheritance)) {
                $tpl->_inheritance = $parent->_inheritance;
            } else {
                unset($tpl->_inheritance);
            }
        } else {
            $tpl = clone $parent;
            $tpl->parent = $parent;
            if (!isset($tpl->templateId) || $tpl->templateId !== $_templateId) {
                $tpl->templateId = $_templateId;
                $tpl->template_resource = $template;
                $tpl->cache_id = $cache_id;
                $tpl->compile_id = $compile_id;
                // $uid is set if template is inline
                if (isset($uid)) {
                    $this->setSourceByUid($tpl, $uid);
                    // if template is called multiple times set flag to to cache template objects
                    $cache_tpl_obj = $cache_tpl_obj || (isset($tpl->compiled->includes[$tpl->template_resource]) &&
                            $tpl->compiled->includes[$tpl->template_resource] > 1);
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
            $tpl->cached = $parent->cached;
        }
        // get variables from calling scope
        if ($parent_scope != Smarty::SCOPE_LOCAL) {
            if ($parent_scope == Smarty::SCOPE_PARENT) {
                $tpl->tpl_vars = &$parent->tpl_vars;
                $tpl->config_vars = &$parent->config_vars;
            } elseif ($parent_scope == Smarty::SCOPE_GLOBAL) {
                $tpl->tpl_vars = &Smarty::$global_tpl_vars;
                $tpl->config_vars = $parent->config_vars;
            } elseif ($parent_scope == Smarty::SCOPE_ROOT) {
                $ptr = $tpl->parent;
                while (!empty($ptr->parent)) {
                    $ptr = $ptr->parent;
                }
                $tpl->tpl_vars = &$ptr->tpl_vars;
                $tpl->config_vars = &$ptr->config_vars;
            } else {
                $tpl->tpl_vars = $parent->tpl_vars;
                $tpl->config_vars = $parent->config_vars;
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
}
