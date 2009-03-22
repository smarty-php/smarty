<?php
/**
* Smarty plugin
* 
* @package Smarty
* @subpackage PluginsConfiguration
* @author Uwe Tews 
*/ 
    define('SMARTY_PHP_PASSTHRU',   0);
    define('SMARTY_PHP_QUOTE', 1);
    define('SMARTY_PHP_REMOVE', 2);
    define('SMARTY_PHP_ALLOW', 3);
/**
* This class does contain the security settings
*/
class Smarty_Security_Policy {
    /**
    * This determines how Smarty handles "<?php ... ?>" tags in templates.
    * possible values:
    * <ul>
    *   <li>SMARTY_PHP_PASSTHRU -> echo PHP tags as they are</li>
    *   <li>SMARTY_PHP_QUOTE    -> escape tags as entities</li>
    *   <li>SMARTY_PHP_REMOVE   -> remove php tags</li>
    *   <li>SMARTY_PHP_ALLOW    -> execute php tags</li>
    * </ul>
    * 
    * @var integer 
    */
    public $php_handling = SMARTY_PHP_REMOVE;

    /**
    * This is the list of template directories that are considered secure.
    * One directory per array element. 
    * $template_dir is in this list implicitly.
    * 
    * @var array 
    */
    public $secure_dir = array();


    /**
    * This is an array of directories where trusted php scripts reside.
    * {@link $security} is disabled during their inclusion/execution.
    * 
    * @var array 
    */
    public $trusted_dir = array();


    /**
    * This is an array of trusted PHP functions.
    *
    * If empty all functions are allowed.
    * If set to 'none' none is allowed.
    * @var array 
    */
    public $php_functions = array('isset', 'empty',
            'count', 'sizeof','in_array', 'is_array','time');

    /**
    * This is an array of trusted modifers.
    *
    * If empty all modifiers are allowed.
    * If set to 'none' none is allowed.
    * @var array 
    */
    public $modifiers = array('escape','count');
    /**
    + flag if constants can be accessed from template
    */
    public $allow_constants = true;
} 

?>
