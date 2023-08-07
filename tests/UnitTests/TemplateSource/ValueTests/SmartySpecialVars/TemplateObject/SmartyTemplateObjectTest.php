<?php
/**
 * Smarty PHPunit tests {$smarty.template_objects}
 *

 * @author  Uwe Tews
 */

/**
 * class for {$smarty.template_objects} tests
 *
 *
 * 
 *
 */
class SmartyTemplateObjectTest extends PHPUnit_Smarty
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
     * test {$smarty.template_objects}
     *
     */
    public function testSmartyTempalteObject() {
        $this->assertEquals('okay', $this->smarty->fetch('template_object.tpl'));
    }
 }
