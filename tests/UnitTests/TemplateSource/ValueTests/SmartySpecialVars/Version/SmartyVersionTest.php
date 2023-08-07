<?php
/**
 * Smarty PHPunit tests {$smarty.version}
 *

 * @author  Uwe Tews
 */

/**
 * class for {$smarty.version} tests
 *
 * 
 * 
 * 
 */
class SmartyVersionTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(__DIR__);
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
        $this->assertEquals(\Smarty\Smarty::SMARTY_VERSION, $this->smarty->fetch('version.tpl'));
    }
 }
