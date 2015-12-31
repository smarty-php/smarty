<?php
/**
 * Smarty PHPunit tests {$smarty.version}
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {$smarty.version} tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class SmartyVersionTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
    }

    public function testInit()
    {
        $this->cleanDirs();
    }
    /**
     * test {$smarty.version}
     *
     */
    public function testSmartyVersion() {
        $this->assertEquals(Smarty::SMARTY_VERSION, $this->smarty->fetch('version.tpl'));
    }
 }
