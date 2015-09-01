<?php

/**
 * Runtime Method renderInline
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 */
class Smarty_Internal_Runtime_Inline
{
    /**
     * Template code runtime function to render inline subtemplate
     *
     * @param Smarty_Internal_Template $callerTpl
     * @param string                   $template       template name
     * @param mixed                    $cache_id       cache id
     * @param mixed                    $compile_id     compile id
     * @param integer                  $caching        cache mode
     * @param integer                  $cache_lifetime life time of cache data
     * @param array                    $data           passed parameter template variables
     * @param int                      $parent_scope   scope in which {include} should execute
     * @param bool                     $cache_tpl_obj  cache template object
     * @param bool                     $isChild        flag if subtemplate is an inheritance child
     * @param string                   $content_func   name of content function
     * @param string                   $uid            source uid
     *
     * @throws \Exception
     */
    public function renderInline(Smarty_Internal_Template $callerTpl, $template, $cache_id, $compile_id,
                                                 $caching, $cache_lifetime, $data, $parent_scope, $cache_tpl_obj,
                                                 $isChild, $content_func, $uid)
    {
        // call runtime extension
        /* @var Smarty_Internal_Template $tpl */
        $tpl = $callerTpl->_Subtemplate->setupSubtemplate($callerTpl, $template, $cache_id, $compile_id,
                                                                         $caching, $cache_lifetime, $data,
                                                                         $parent_scope, $cache_tpl_obj, $uid);
        $tpl->isChild = $isChild;
        if ($callerTpl->smarty->debugging) {
            $callerTpl->smarty->_debug->start_template($tpl);
            $callerTpl->smarty->_debug->start_render($tpl);
        }
        $tpl->compiled->getRenderedTemplateCode($tpl, $content_func);
        if ($callerTpl->smarty->debugging) {
            $callerTpl->smarty->_debug->end_template($tpl);
            $callerTpl->smarty->_debug->end_render($tpl);
        }
        if ($caching == 9999 && $tpl->compiled->has_nocache_code) {
            $callerTpl->cached->hashes[$tpl->compiled->nocache_hash] = true;
        }
        if (!isset($callerTpl->_Block) && $isChild && isset($tpl->_Block) &&
            ($callerTpl->isChild || !empty($callerTpl->source->components))
        ) {
            $callerTpl->_Block = $tpl->_Block;
        }
    }

}