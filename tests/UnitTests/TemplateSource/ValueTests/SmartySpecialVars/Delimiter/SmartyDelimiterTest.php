<?php
/**
 * Smarty PHPunit tests {$smarty.ldelim} {$smarty.rdelim}
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {$smarty.ldelim} {$smarty.rdelim} tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class SmartyDelimiterTest extends PHPUnit_Smarty
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
     * test {$smarty.ldelim} {$smarty.rdelim}
     *
     */
    public function testSmartyDelimiter() {
        $this->assertEquals('left = { right = }', $this->smarty->fetch('delimiter.tpl'));
    }
 }
