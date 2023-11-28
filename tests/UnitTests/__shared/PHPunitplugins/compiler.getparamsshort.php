<?php
/**
 * Smarty plugin params
 *


 */

use Smarty\Compile\Base;

/**
 * Smarty {getparams}
 *
 * @param array  $params   parameter array
 * @param object $template template object
 *
 * @return string
 */
class smarty_compiler_getparamsshort extends Base
{
    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Base
     */
    public $required_attributes = array();

    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Base
     */
    public $optional_attributes = array('_any');
    /**
     * Attribute definition: Overwrites base class.
     *
     * @var array
     * @see Base
     */
    public $shorttag_order = array('s1', 's2', 's3');

    public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null): string
	{
        $_attr = $this->getAttributes($compiler, $args);
        $output = '<?php echo "array(';
             foreach ($_attr as $key => $value) {
                $output .= "'{$key}'=>\" . ";
                $output .= is_string($value) ? "({$value})" : ("'" . var_export($value, true). "'");
                $output .= ' . ",';

            }

        $output .= ")\";?>\n";
        return $output;
    }
}
