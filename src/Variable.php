<?php

namespace Smarty;

/**
 * class for the Smarty variable object
 * This class defines the Smarty variable object
 *


 */
#[\AllowDynamicProperties]
class Variable
{
    /**
     * template variable
     *
     * @var mixed
     */
    public $value = null;

    /**
     * if true any output of this variable will be not cached
     *
     * @var boolean
     */
    public $nocache = false;

    /**
     * create Smarty variable object
     *
     * @param mixed   $value   the value to assign
     * @param boolean $nocache if true any output of this variable will be not cached
     */
    public function __construct($value = null, $nocache = false)
    {
        $this->value = $value;
        $this->nocache = $nocache;
    }

	public function getValue() {
		return $this->value;
	}

    /**
     * <<magic>> String conversion
     *
     * @return string
     */
    public function __toString()
    {
        return (string)$this->value;
    }
}
