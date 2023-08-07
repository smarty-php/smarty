<?php
/**
 * Smarty PHPunit tests of generic getter/setter
 *

 * @author  Uwe Tews
 */

/**
 * class for generic getter/setter tests
 *
 *
 * 
 * 
 */
class GetterSetterTest extends PHPUnit_Smarty
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
     * test setter on Smarty object
     */
    public function testSmartySetter()
    {
        $this->smarty->setLeftDelimiter('<{');
        $this->smarty->setRightDelimiter('}>');
        $this->assertEquals('<{', $this->smarty->getLeftDelimiter());
        $this->assertEquals('}>', $this->smarty->getRightDelimiter());
    }

    /**
     * test getter on Smarty object
     */
    public function testSmartyGetter()
    {
        $this->smarty->setLeftDelimiter('<{');
        $this->smarty->setRightDelimiter('}>');
        $this->assertEquals('<{', $this->smarty->getLeftDelimiter());
        $this->assertEquals('}>', $this->smarty->getRightDelimiter());
    }

    /**
     * test setter on Template object
     */
    public function testTemplateSetter()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $tpl->setLeftDelimiter('<{');
        $tpl->setRightDelimiter('}>');
        $this->assertEquals('<{', $tpl->getLeftDelimiter());
        $this->assertEquals('}>', $tpl->getRightDelimiter());
        $this->assertEquals('{', $this->smarty->getLeftDelimiter());
        $this->assertEquals('}', $this->smarty->getRightDelimiter());
    }

    /**
     * test getter on Template object
     */
    public function testTemplateGetter()
    {
        $tpl = $this->smarty->createTemplate('helloworld.tpl');
        $tpl->setLeftDelimiter('<{');
        $tpl->setRightDelimiter('}>');
        $this->assertEquals('<{', $tpl->getLeftDelimiter());
        $this->assertEquals('}>', $tpl->getRightDelimiter());
    }
}
