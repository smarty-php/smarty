<?php

/**
 * Runtime Method _getSubTemplate
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 *
 **/
class Smarty_Internal_Runtime_Subtemplate
{
    /**
     * Template code runtime function to get subtemplate content
     *
     * @param \Smarty_Internal_Template $callerTpl      template object of caller
     * @param string                    $template       template name
     * @param mixed                     $cache_id       cache id
     * @param mixed                     $compile_id     compile id
     * @param integer                   $caching        cache mode
     * @param integer                   $cache_lifetime life time of cache data
     * @param array                     $data           passed parameter template variables
     * @param int                       $parent_scope   scope in which {include} should execute
     * @param bool                      $cache_tpl_obj  cache template object
     * @param bool                      $isChild        flag if subtemplate is an inheritance child
     *
     * @throws \SmartyException
     */
    public function renderSubtemplate(Smarty_Internal_Template $callerTpl, $template, $cache_id, $compile_id, $caching,
                                      $cache_lifetime, $data, $parent_scope, $cache_tpl_obj, $isChild)
    {
        $tpl = $this->setupSubtemplate($callerTpl, $template, $cache_id, $compile_id, $caching, $cache_lifetime, $data,
                                       $parent_scope, $cache_tpl_obj);
        $tpl->isChild = $isChild;
        $tpl->render();
        if ($tpl->isChild && !isset($callerTpl->_Block) && isset($tpl->_Block) &&
            ($callerTpl->isChild || !empty($callerTpl->source->components))
        ) {
            $callerTpl->_Block = $tpl->_Block;
        }
    }

    /**
     * Template code runtime function to set up an inline subtemplate
     *
     * @param \Smarty_Internal_Template $callerTpl
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
    public function setupSubtemplate(Smarty_Internal_Template $callerTpl, $template, $cache_id, $compile_id, $caching,
                                     $cache_lifetime, $data, $parent_scope, $cache_tpl_obj, $uid = null)
    {
        $_templateId = isset($callerTpl->smarty->_cache['template_objects']) ?
            $callerTpl->smarty->_getTemplateId($template, $cache_id, $compile_id) : null;
        // already in template cache?
        /* @var Smarty_Internal_Template $tpl */
        if (isset($callerTpl->smarty->_cache['template_objects'][$_templateId])) {
            // clone cached template object because of possible recursive call
            $tpl = clone $callerTpl->smarty->_cache['template_objects'][$_templateId];
            $tpl->parent = $callerTpl;
            if ((bool) $tpl->caching !== (bool) $caching) {
                unset($tpl->compiled);
            }
            // get variables from calling scope
            if ($parent_scope == Smarty::SCOPE_LOCAL) {
                $tpl->tpl_vars = $callerTpl->tpl_vars;
                $tpl->config_vars = $callerTpl->config_vars;
            }
            $tpl->tpl_function = $callerTpl->tpl_function;
            //if (isset($callerTpl->_cache['inheritanceBlocks'])) {
            //    $tpl->_cache['inheritanceBlocks'] = $callerTpl->_cache['inheritanceBlocks'];
            //}
        } else {
            $tpl = clone $callerTpl;
            $tpl->parent = $callerTpl;
            $tpl->isChild = false;
            if (!isset($tpl->templateId) || $tpl->templateId !== $_templateId) {
                $tpl->templateId = $_templateId;
                $tpl->template_resource = $template;
                $tpl->cache_id = $cache_id;
                $tpl->compile_id = $compile_id;
                if (isset($uid)) {
                    $tpl->compiled = $callerTpl->compiled;
                    if (isset($tpl->compiled->includes["{$tpl->source->type}:{$tpl->source->name}"]) &&
                        $tpl->compiled->includes["{$tpl->source->type}:{$tpl->source->name}"] > 1
                    ) {
                        $cache_tpl_obj = true;
                    }
                    if (isset($tpl->compiled->file_dependency[$uid])) {
                        $info = $tpl->compiled->file_dependency[$uid];
                        $tpl->source =
                            new Smarty_Template_Source(isset($tpl->smarty->_cache['resource_handlers'][$info[2]]) ?
                                                           $tpl->smarty->_cache['resource_handlers'][$info[2]] :
                                                           Smarty_Resource::load($tpl->smarty, $info[2]), $tpl->smarty,
                                                       $info[0], $info[2], $info[0]);
                        $tpl->source->filepath = $info[0];
                        $tpl->source->timestamp = $info[1];
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
            $tpl->cached = $callerTpl->cached;
        }
        // get variables from calling scope
        if ($parent_scope != Smarty::SCOPE_LOCAL) {
            if ($parent_scope == Smarty::SCOPE_PARENT) {
                $tpl->tpl_vars = &$callerTpl->tpl_vars;
                $tpl->config_vars = &$callerTpl->config_vars;
            } elseif ($parent_scope == Smarty::SCOPE_GLOBAL) {
                $tpl->tpl_vars = &Smarty::$global_tpl_vars;
                $tpl->config_vars = $callerTpl->config_vars;
            } elseif ($parent_scope == Smarty::SCOPE_ROOT) {
                $ptr = $tpl->parent;
                while (!empty($ptr->parent)) {
                    $ptr = $ptr->parent;
                }
                $tpl->tpl_vars = &$ptr->tpl_vars;
                $tpl->config_vars = &$ptr->config_vars;
            } else {
                $tpl->tpl_vars = $callerTpl->tpl_vars;
                $tpl->config_vars = $callerTpl->config_vars;
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