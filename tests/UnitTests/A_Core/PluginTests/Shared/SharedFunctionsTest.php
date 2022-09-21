<?php
/**
 * Smarty PHPunit tests of shared plugin functions
 *
 * @package PHPunit
 * @author  Rodney Rehm
 */

/**
 * class for function tests
 */
class SharedFunctionsTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    /**
     * test smarty_function_escape_special_chars()
     */
    public function testEscapeSpecialChars()
    {
        require_once SMARTY_PLUGINS_DIR . 'shared.escape_special_chars.php';

        $this->assertEquals('hello&lt;world &copy;', smarty_function_escape_special_chars('hello<world &copy;'));
        $this->assertEquals('ö€', smarty_function_escape_special_chars('ö€'));
    }
}
