<?php

/**
 * Inline Runtime Methods render
 *
 * @package    Smarty
 * @subpackage PluginsInternal
 * @author     Uwe Tews
 *
 **/
class Smarty_Internal_Runtime_Inline extends Smarty_Internal_Runtime_Subtemplate
{

    /**
     * Template code runtime function to set up an inline subtemplate
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
     * @param string                    $uid            file dependency uid
     * @param string                    $content_func   function name
     *
     * @throws \Exception
     */
    public function render(\Smarty_Internal_Template $parent, $template, $cache_id, $compile_id, $caching,
                           $cache_lifetime, $data, $parent_scope, $cache_tpl_obj, $uid, $content_func)
    {
        $tpl = $this->setupSubtemplate($parent, $template, $cache_id, $compile_id, $caching, $cache_lifetime, $data,
                                       $parent_scope, $cache_tpl_obj, $uid);
        if ($parent->smarty->debugging) {
            $parent->smarty->_debug->start_template($tpl);
            $parent->smarty->_debug->start_render($tpl);
        }
        $tpl->compiled->getRenderedTemplateCode($tpl, $content_func);
        if ($parent->smarty->debugging) {
            $parent->smarty->_debug->end_template($tpl);
            $parent->smarty->_debug->end_render($tpl);
        }
        if ($tpl->caching == 9999 && $tpl->compiled->has_nocache_code) {
            $parent->cached->hashes[$tpl->compiled->nocache_hash] = true;
        }
    }

    /**
     * Set source object of inline template by $uid
     *
     * @param \Smarty_Internal_Template $tpl
     * @param  string                   $uid
     *
     * @throws \SmartyException
     */
    public function setSourceByUid(Smarty_Internal_Template $tpl, $uid)
    {
        // inline templates have same compiled resource
        $tpl->compiled = $tpl->parent->compiled;
        if (isset($tpl->compiled->file_dependency[$uid])) {
            list($filepath, $timestamp, $resource) = $tpl->compiled->file_dependency[$uid];
            $tpl->source = new Smarty_Template_Source(isset($tpl->smarty->_cache['resource_handlers'][$resource]) ?
                                                          $tpl->smarty->_cache['resource_handlers'][$resource] :
                                                          Smarty_Resource::load($tpl->smarty, $resource), $tpl->smarty,
                                                      $filepath, $resource, $filepath);
            $tpl->source->filepath = $filepath;
            $tpl->source->timestamp = $timestamp;
            $tpl->source->exists = true;
            $tpl->source->uid = $uid;
        } else {
            $tpl->source = null;
        }
    }
}
