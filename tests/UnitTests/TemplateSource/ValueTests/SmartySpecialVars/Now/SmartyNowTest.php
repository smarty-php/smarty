<?php
/**
 * Smarty PHPunit tests {$smarty.now}
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for {$smarty.now} tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class SmartyNowTest extends PHPUnit_Smarty
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
     * test {$smarty.now}
     *
     */
    public function testSmartyNow() {
        $result = $this->smarty->fetch('now.tpl');
        $this->assertTrue(is_numeric($result));
        $this->assertTrue((time() - $result) <= 1);
    }
    /**
     * test {$smarty.now nocache}
     * @group slow
     */
    public function testSmartyNowNocache() {
        $this->smarty->setCaching(true);
        $result = $this->smarty->fetch('now_nocache.tpl');
        $this->assertTrue(is_numeric($result));
        $this->assertTrue((time() - $result) <= 1);
        sleep(2);
        $result2 = $this->smarty->fetch('now_nocache.tpl');
        $this->assertTrue(is_numeric($result2));
        $this->assertTrue((time() - $result2) <= 1);
    }
}
