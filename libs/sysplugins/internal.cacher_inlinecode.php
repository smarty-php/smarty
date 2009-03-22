<?php

/**
* Smarty Internal Plugin Cacher InlineCode
* 
* Process nocached code.
* Version to inject nocache code directly into cache file
* if caching is disabled at render time the code is being evaluated
* 
* @package Smarty
* @subpackage Cacher
* @author Uwe Tews 
*/

/**
* Smarty Internal Plugin Cacher InlineCode Class
*/
class Smarty_Internal_Cacher_InlineCode extends Smarty_Internal_PluginBase {
    /**
    * Inject inline code for nocache template sections
    * 
    * This method gets the content of each template element from the parser.
    * If the content is compiled code and it should be not cached the code is injected
    * into the rendered output.
    * 
    * @param string $content content of template element
    * @param object $compiler intance of compiler class
    * @param boolean $tag_nocache true if the parser detected a nocache situation
    * @param boolean $is_code true if content is compiled code
    * @return string content
    */
    public function processNocacheCode ($content, $compiler, $tag_nocache, $is_code)
    {
        // If the template is not evaluated and we have a nocache section and or a nocache tag
        if ($is_code) {
            // generate replacement code
            if (!$compiler->template->isEvaluated() && $compiler->template->caching &&
                    ($tag_nocache || $compiler->nocache || $compiler->tag_nocache)) {
                $compiler->tag_nocache = false;
                $_output = str_replace("'", "\'", $content);
                $_output = '<?php  echo \'' . $_output . '\';?>';
            } else {
                $_output = $content;
            } 
        } else {
            $_output = $content;
        } 
        // if compiled code shall be grabbed
        if ($compiler->template->extract_code == false) {
            // return output
            return $_output;
        } else {
            // store code in extract buffer
            $compiler->template->extracted_compiled_code .= $_output;
            return '';
        } 
    } 

    /**
    * Initialize cacher
    * 
    * Is a noop in current implementation
    * 
    * @param object $compiler intance of compiler class
    */
    public function initCacher ($compiler)
    {
        return;
    } 

    /**
    * Close cacher
    * 
    * Hook to perform any post processing on the final compiled template
    * Is a noop in current implementation
    * 
    * @param object $compiler intance of compiler class
    * @param string $template_code complete compiled template
    * @return string compiled template output
    */
    public function closeCacher ($compiler, $template_code)
    {
        return $template_code;
    } 

    /**
    * Retrieve cached output
    * 
    * Calls the cache resource according to the caching type
    * 
    * @param object $_template intance of template object
    * @return string content from cache
    */
    public function getCachedContents ($_template)
    {
        return $this->smarty->cache_resource_objects[$_template->caching_type]->getCachedContents($_template);
    } 

    /**
    * Store cached output
    * 
    * Calls the cache resource according to the caching type
    * 
    * @param object $_template intance of template object
    * @return boolean status
    */
    public function writeCachedContent ($_template)
    {
        return $this->smarty->cache_resource_objects[$_template->caching_type]->writeCachedContent($_template);
    } 
} 

?>
