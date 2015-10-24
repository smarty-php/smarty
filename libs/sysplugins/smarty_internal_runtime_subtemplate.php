<?php

/**
 * Sub Template Runtime Methods render, setupSubTemplate
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 *
 **/
class Smarty_Internal_Runtime_SubTemplate
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
     * @param int                       $scope          scope in which {include} should execute
     * @param bool                      $forceTplCache  cache template object
     *
     */
    public function render(Smarty_Internal_Template $parent, $template, $cache_id, $compile_id, $caching,
                           $cache_lifetime, $data, $scope, $forceTplCache)
    {
        $tpl = $this->setupSubTemplate($parent, $template, $cache_id, $compile_id, $caching, $cache_lifetime, $data,
                                       $scope);
        $tpl->render();
        $this->updateTemplateCache($tpl, $forceTplCache);
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
     * @param int                       $scope          scope in which {include} should execute
     * @param string|null               $uid            source uid
     *
     * @return \Smarty_Internal_Template template object
     * @throws \SmartyException
     */
    public function setupSubTemplate(Smarty_Internal_Template $parent, $template, $cache_id, $compile_id, $caching,
                                     $cache_lifetime, $data, $scope, $uid = null)
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
            if ($scope == Smarty::SCOPE_LOCAL) {
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
                } else {
                    $tpl->source = null;
                    unset($tpl->compiled);
                }
                if (!isset($tpl->source)) {
                    $tpl->source = Smarty_Template_Source::load($tpl);
                }
                unset($tpl->cached);
            }
        }
        $tpl->caching = $caching;
        $tpl->cache_lifetime = $cache_lifetime;
        if ($caching == 9999) {
            $tpl->cached = $parent->cached;
        }
        // get variables from calling scope
        if ($scope != $tpl->scope) {
            if ($tpl->scope != Smarty::SCOPE_LOCAL) {
                unset($tpl->tpl_vars, $tpl->config_vars);
                $tpl->tpl_vars = array();
                $tpl->config_vars = array();
            }
            if ($scope == Smarty::SCOPE_PARENT) {
                $tpl->tpl_vars = &$parent->tpl_vars;
                $tpl->config_vars = &$parent->config_vars;
            } elseif ($scope == Smarty::SCOPE_GLOBAL) {
                $tpl->tpl_vars = &Smarty::$global_tpl_vars;
                $tpl->config_vars = $parent->config_vars;
            } elseif ($scope == Smarty::SCOPE_ROOT) {
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
            $tpl->scope = $scope;
        }

        if (!empty($data)) {
            // set up variable values
            foreach ($data as $_key => $_val) {
                $tpl->tpl_vars[$_key] = new Smarty_Variable($_val);
            }
        }
        return $tpl;
    }

    public function updateTemplateCache(Smarty_Internal_Template $tpl, $forceTplCache)
    {
        if (isset($tpl->smarty->_cache['template_objects'][$tpl->_getTemplateId()]) ||
            $tpl->source->handler->recompiled
        ) {
            return;
        }
        // if template is called multiple times set flag to to cache template objects
        $forceTplCache = $forceTplCache || (isset($tpl->compiled->includes[$tpl->template_resource]) &&
                $tpl->compiled->includes[$tpl->template_resource] > 1);
        // check if template object should be cached
        if ((isset($tpl->parent->templateId) &&
            isset($tpl->smarty->_cache['template_objects'][$tpl->parent->templateId]) ||
            ($forceTplCache && $tpl->smarty->resource_cache_mode & Smarty::RESOURCE_CACHE_AUTOMATIC) ||
            $tpl->smarty->resource_cache_mode & Smarty::RESOURCE_CACHE_ON)
        ) {
            $tpl->parent = null;
            if ($tpl->scope != Smarty::SCOPE_LOCAL) {
                unset($tpl->tpl_vars, $tpl->config_vars);
                $tpl->scope = Smarty::SCOPE_LOCAL;
            }
            $tpl->tpl_vars = array();
            $tpl->config_vars = array();
            $tpl->smarty->_cache['template_objects'][$tpl->_getTemplateId()] = $tpl;
        }
    }
}
