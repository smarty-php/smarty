<?php
/**
* Smarty plugin
* 
* @package Smarty
* @subpackage Security
* @author Uwe Tews 
*/ 

/**
* This class does contain the security settings
*/
class Smarty_Security {
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
    public $php_handling = SMARTY_PHP_PASSTHRU;

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
    * This is an array of trusted static classes.
    *
    * If empty access to all static classes is allowed.
    * If set to 'none' none is allowed.
    * @var array 
    */
    public $static_classes = array();

    /**
    * This is an array of trusted PHP functions.
    *
    * If empty all functions are allowed.
    * If set to 'none' none is allowed.
    * @var array 
    */
    public $php_functions = array('isset', 'empty',
            'count', 'sizeof','in_array', 'is_array','time','nl2br');

    /**
    * This is an array of trusted modifers.
    *
    * If empty all modifiers are allowed.
    * If set to 'none' none is allowed.
    * @var array 
    */
    public $modifiers = array('escape','count');

    /**
    * This is an array of trusted streams.
    *
    * If empty all streams are allowed.
    * If set to 'none' none is allowed.
    * @var array 
    */
    public $streams = array('file');
    /**
    + flag if constants can be accessed from template
    */
    public $allow_constants = true;
    /**
    + flag if super globals can be accessed from template
    */
    public $allow_super_globals = true;
    /**
    + flag if {php} tag can be executed
    */
    public $allow_php_tag = false;
} 

?>