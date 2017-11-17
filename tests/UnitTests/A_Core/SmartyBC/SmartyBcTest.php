<?php
/**
 * Smarty PHPunit tests SmartyBC code
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class SmartyBC class tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class SmartyBcTest extends PHPUnit_Smarty
{
    public $loadSmartyBC = true;
    public $loadSmarty = false;
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test {php} tag
     */
    public function testSmartyPhpTag()
    {
        $this->assertEquals('hello world', $this->smartyBC->fetch('string:{php} echo "hello world"; {/php}'));
    }
}
