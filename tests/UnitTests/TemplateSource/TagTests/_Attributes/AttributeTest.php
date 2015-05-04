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
 * @backupStaticAttributes enabled
 */
class AttributeTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);
    }


    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Syntax error in template "b8ecd121bbbc031241b1116a9db691a759eceadf"
     * @expectedExceptionMessage missing "var" attribute
     * test required attribute
     */
    public function testRequiredAttributeVar()
    {
        $this->smarty->fetch('string:{assign value=1}');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Syntax error in template "46d3649920e0043f055702ef3ceef0ecdc44b892"
     * @expectedExceptionMessage unexpected "bar" attribute
     * test unexpected attribute
     */
    public function testUnexpectedAttribute()
    {
        $this->smarty->fetch('string:{assign var=foo value=1 bar=2}');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Syntax error in template "d6c824b50e89d8fe12b393ae8ab68daeb7b6c240"
     * @expectedExceptionMessage illegal value of option flag "nocache"
     * test illegal option value
     */
    public function testIllegalOptionValue()
    {
        $this->smarty->fetch('string:{assign var=foo value=1 nocache=buh}');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Syntax error in template "a119616ffa139e7b1145b1cd36adbff7bc9be7cf"
     * @expectedExceptionMessage too many shorthand attributes
     * test too many shorthands
     */
    public function testTooManyShorthands()
    {
        $this->smarty->fetch('string:{assign foo 1 2}');
    }
}
