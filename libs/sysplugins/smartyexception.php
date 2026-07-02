<?php

/**
 * Smarty exception class
 *
 * @package Smarty
 */
// PHP 8.2+: Allow dynamic properties for internal state and extensibility
#[\AllowDynamicProperties]
class SmartyException extends Exception
{
    public static $escape = false;

    /**
     * @return string
     */
    public function __toString()
    {
        return ' --> Smarty: ' . (self::$escape ? htmlentities($this->message) : $this->message) . ' <-- ';
    }
}
