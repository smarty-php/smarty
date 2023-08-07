<?php
/**
 * Smarty PHPunit tests {$smarty.ldelim} {$smarty.rdelim}
 *

 * @author  Uwe Tews
 */

/**
 * class for {$smarty.ldelim} {$smarty.rdelim} tests
 *
 * 
 * 
 * 
 */
class SmartyDelimiterTest extends PHPUnit_Smarty
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
     * test {$smarty.ldelim} {$smarty.rdelim}
     *
     */
    public function testSmartyDelimiter() {
        $this->assertEquals('left = { right = }', $this->smarty->fetch('delimiter.tpl'));
    }
 }
