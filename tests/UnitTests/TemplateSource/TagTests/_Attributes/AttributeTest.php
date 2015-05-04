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
     * @expectedExceptionMessage Syntax Error in template &quot;b8ecd121bbbc031241b1116a9db691a759eceadf&quot;
     * @expectedExceptionMessage missing &quot;var&quot; attribute
     * test required attribute
     */
    public function testRequiredAttributeVar()
    {
        $this->smarty->fetch('string:{assign value=1}');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Syntax Error in template &quot;46d3649920e0043f055702ef3ceef0ecdc44b892&quot;
     * @expectedExceptionMessage unexpected &quot;bar&quot; attribute
     * test unexpected attribute
     */
    public function testUnexpectedAttribute()
    {
        $this->smarty->fetch('string:{assign var=foo value=1 bar=2}');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Syntax Error in template &quot;d6c824b50e89d8fe12b393ae8ab68daeb7b6c240&quot;
     * @expectedExceptionMessage illegal value of option flag &quot;nocache&quot;
     * test illegal option value
     */
    public function testIllegalOptionValue()
    {
        $this->smarty->fetch('string:{assign var=foo value=1 nocache=buh}');
    }

    /**
     * @expectedException        SmartyCompilerException
     * @expectedExceptionMessage Syntax Error in template &quot;a119616ffa139e7b1145b1cd36adbff7bc9be7cf&quot;
     * @expectedExceptionMessage too many shorthand attributes
     * test too many shorthands
     */
    public function testTooManyShorthands()
    {
        $this->smarty->fetch('string:{assign foo 1 2}');
    }
}
