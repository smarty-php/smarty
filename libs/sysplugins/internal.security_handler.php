<?php
/**
* Smarty Internal Plugin Security Handler
* 
* @package Smarty
* @subpackage Security
* @author Uwe Tews 
*/
/**
* This class contains all methods for security checking
*/
class Smarty_Internal_Security_Handler {
    function __construct($smarty)
    {
        $this->smarty = $smarty;
    } 
    /**
    * Check if PHP function is trusted.
    * 
    * @param string $function_name 
    * @param object $compiler compiler object
    * @return boolean true if function is trusted
    */
    function isTrustedPhpFunction($function_name, $compiler)
    {
        if (empty($this->smarty->security_policy->php_functions) || in_array($function_name, $this->smarty->security_policy->php_functions)) {
            return true;
        } else {
            $compiler->trigger_template_error ("PHP function \"" . $function_name . "\" not allowed by security setting");
            return false;
        } 
    } 

    /**
    * Check if modifier is trusted.
    * 
    * @param string $modifier_name 
    * @param object $compiler compiler object
    * @return boolean true if modifier is trusted
    */
    function isTrustedModifier($modifier_name, $compiler)
    {
        if (empty($this->smarty->security_policy->modifiers) || in_array($modifier_name, $this->smarty->security_policy->modifiers)) {
            return true;
        } else {
            $compiler->trigger_template_error ("modifier \"" . $modifier_name . "\" not allowed by security setting");
            return false;
        } 
    } 
    /**
    * Check if stream is trusted.
    * 
    * @param string $stream_name 
    * @param object $compiler compiler object
    * @return boolean true if stream is trusted
    */
    function isTrustedStream($stream_name)
    {
        if (empty($this->smarty->security_policy->streams) || in_array($stream_name, $this->smarty->security_policy->streams)) {
            return true;
        } else {
            throw new Exception ("stream \"" . $stream_name . "\" not allowed by security setting");
            return false;
        } 
    } 

    /**
    * Check if directory of file resource is trusted.
    * 
    * @param string $filepath 
    * @param object $compiler compiler object
    * @return boolean true if directory is trusted
    */
    function isTrustedResourceDir($filepath)
    {
        $_rp = realpath($filepath);
        if (isset($this->smarty->template_dir)) {
            foreach ((array)$this->smarty->template_dir as $curr_dir) {
                if (($_cd = realpath($curr_dir)) !== false &&
                        strncmp($_rp, $_cd, strlen($_cd)) == 0 &&
                        (strlen($_rp) == strlen($_cd) || substr($_rp, strlen($_cd), 1) == DS)) {
                    return true;
                } 
            } 
        } 
        if (!empty($this->smarty->security_policy->secure_dir)) {
            foreach ((array)$this->smarty->security_policy->secure_dir as $curr_dir) {
                if (($_cd = realpath($curr_dir)) !== false) {
                    if ($_cd == $_rp) {
                        return true;
                    } elseif (strncmp($_rp, $_cd, strlen($_cd)) == 0 &&
                            (strlen($_rp) == strlen($_cd) || substr($_rp, strlen($_cd), 1) == DS)) {
                        return true;
                    } 
                } 
            } 
        } 

        throw new Exception ("directory \"" . $_rp . "\" not allowed by security setting");
        return false;
    } 
    /**
    * Check if directory of file resource is trusted.
    * 
    * @param string $filepath 
    * @param object $compiler compiler object
    * @return boolean true if directory is trusted
    */
    function isTrustedPHPDir($filepath)
    {
        $_rp = realpath($filepath);
        if (!empty($this->smarty->security_policy->trusted_dir)) {
            foreach ((array)$this->smarty->security_policy->trusted_dir as $curr_dir) {
                if (($_cd = realpath($curr_dir)) !== false) {
                    if ($_cd == $_rp) {
                        return true;
                    } elseif (strncmp($_rp, $_cd, strlen($_cd)) == 0 &&
                            substr($_rp, strlen($_cd), 1) == DS) {
                        return true;
                    } 
                } 
            } 
        } 

        throw new Exception ("directory \"" . $_rp . "\" not allowed by security setting");
        return false;
    } 
} 

?>
