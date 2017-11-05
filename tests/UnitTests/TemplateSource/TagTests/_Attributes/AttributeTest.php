<?php
/**
 * Smarty PHPunit tests for tag attributes
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for tag attribute tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class AttributeTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addPluginsDir("../../../__shared/PHPunitplugins/");
    }


    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage missing 'var' attribute
     * test required attribute
     */
    public function testRequiredAttributeVar()
    {
        $this->smarty->fetch('string:{assign value=1}');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage unexpected 'bar' attribute
     * test unexpected attribute
     */
    public function testUnexpectedAttribute()
    {
        $this->smarty->fetch('string:{assign var=foo value=1 bar=2}');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage for option flag 'nocache'
     * @expectedExceptionMessage illegal value
     * test illegal option value
     */
    public function testIllegalOptionValue()
    {
        $this->smarty->fetch('string:{assign var=foo value=1 nocache=buh}');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage too many shorthand attributes
     * test too many shorthands
     */
    public function testTooManyShorthands()
    {
        $this->smarty->fetch('string:{assign foo 1 2}');
    }

    /**
      */
    public function testNumericParams()
    {
        $this->assertEquals('array(\'a\'=>\'pa\',0=>\'isnull\',)', $this->strip($this->smarty->fetch('string:{getparams a=\'pa\' 0=isnull}')));
    }


}
