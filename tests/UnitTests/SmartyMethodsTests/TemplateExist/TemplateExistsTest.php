<?php
/**
 * Smarty PHPunit tests for templateExists method
 *

 * @author  Uwe Tews
 */

/**
 * class for templateExists tests
 *
 *
 * 
 * 
 */
class TemplateExistsTest extends PHPUnit_Smarty
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
     * test $smarty->templateExists true
     */
    public function testSmartyTemplateExists()
    {
        $this->assertTrue($this->smarty->templateExists('helloworld.tpl'));
    }

    /**
     * test $smarty->templateExists false
     */
    public function testSmartyTemplateNotExists()
    {
        $this->assertFalse($this->smarty->templateExists('notthere.tpl'));
    }
}
