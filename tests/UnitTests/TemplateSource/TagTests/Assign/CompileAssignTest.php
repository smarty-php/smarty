<?php
/**
 * Smarty PHPunit tests compilation of assign tags
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for assign tags tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileAssignTest extends PHPUnit_Smarty
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
     * test old style of assign tag
     */
    public function testAssignOld1()
    {
        $this->assertEquals("1", $this->smarty->fetch('eval:{assign var=foo   value=1}{$foo}'));
        $this->assertEquals("1", $this->smarty->fetch('eval:{assign var = foo   value= 1}{$foo}'));
    }

    public function testAssignOld2()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var=\'foo\' value=1}{$foo}');
        $this->assertEquals("1", $this->smarty->fetch($tpl));
    }

    public function testAssignOld3()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var="foo" value=1}{$foo}');
        $this->assertEquals("1", $this->smarty->fetch($tpl));
    }

    public function testAssignOld4()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value=bar}{$foo}');
        $this->assertEquals("bar", $this->smarty->fetch($tpl));
    }

    public function testAssignOld5()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value=1+2}{$foo}');
        $this->assertEquals("3", $this->smarty->fetch($tpl));
    }

    public function testAssignOld6()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value=strlen(\'bar\')}{$foo}');
        $this->assertEquals("3", $this->smarty->fetch($tpl));
    }

    public function testAssignOld7()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value=\'bar\'|strlen}{$foo}');
        $this->assertEquals("3", $this->smarty->fetch($tpl));
    }

    public function testAssignOld8()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value=[9,8,7,6]}{foreach $foo as $x}{$x}{/foreach}');
        $this->assertEquals("9876", $this->smarty->fetch($tpl));
    }

    public function testAssignOld9()
    {
        $tpl = $this->smarty->createTemplate('eval:{assign var=foo value=[\'a\'=>9,\'b\'=>8,\'c\'=>7,\'d\'=>6]}{foreach $foo as $x}{$x@key}{$x}{/foreach}');
        $this->assertEquals("a9b8c7d6", $this->smarty->fetch($tpl));
    }

    /**
     * test assign tag shorthands
     */
    public function testAssignShort1()
    {
        $this->assertEquals("1", $this->smarty->fetch('string:{assign foo  value=1}{$foo}'));
    }

    public function testAssignShort2()
    {
        $this->assertEquals("1", $this->smarty->fetch('string:{assign foo 1}{$foo}'));
    }

    /**
     * test new style of assign tag
     */
    public function testAssignNew1()
    {
        $this->assertEquals("1", $this->smarty->fetch('eval:{$foo=1}{$foo}'));
        $this->assertEquals("1", $this->smarty->fetch('eval:{$foo =1}{$foo}'));
        $this->assertEquals("1", $this->smarty->fetch('eval:{$foo =  1}{$foo}'));
    }

    public function testAssignNew2()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=bar}{$foo}');
        $this->assertEquals("bar", $this->smarty->fetch($tpl));
    }

    public function testAssignNew3()
    {
        $this->assertEquals("3", $this->smarty->fetch('eval:{$foo=1+2}{$foo}'));
        $this->assertEquals("3", $this->smarty->fetch('eval:{$foo = 1+2}{$foo}'));
        $this->assertEquals("3", $this->smarty->fetch('eval:{$foo = 1 + 2}{$foo}'));
    }

    public function testAssignNew4()
    {
        $tpl = $this->smarty->createTemplate('eval:{$foo=strlen(\'bar\')}{$foo}');
        $this->assertEquals("3", $this->smarty->fetch($tpl));
    }

    public function testAssignNew5()
    {
        $tpl = $this->smarty->createTemplate("eval:{\$foo='bar'|strlen}{\$foo}");
        $this->assertEquals("3", $this->smarty->fetch($tpl));
    }

    public function testAssignNew6()
    {
        $tpl = $this->smarty->createTemplate("eval:{\$foo=[9,8,7,6]}{foreach \$foo as \$x}{\$x}{/foreach}");
        $this->assertEquals("9876", $this->smarty->fetch($tpl));
    }

    public function testAssignNew7()
    {
        $tpl = $this->smarty->createTemplate("eval:{\$foo=['a'=>9,'b'=>8,'c'=>7,'d'=>6]}{foreach \$foo as \$x}{\$x@key}{\$x}{/foreach}");
        $this->assertEquals("a9b8c7d6", $this->smarty->fetch($tpl));
    }

    public function testAssignArrayAppend()
    {
        $tpl = $this->smarty->createTemplate("eval:{\$foo =1}{\$foo[] = 2}{foreach \$foo as \$x}{\$x@key}{\$x}{/foreach}");
        $this->assertEquals("0112", $this->smarty->fetch($tpl));
    }

    public function testAssignArrayAppend2()
    {
        $this->smarty->assign('foo', 1);
        $tpl = $this->smarty->createTemplate("eval:{\$foo[]=2}{foreach \$foo as \$x}{\$x@key}{\$x}{/foreach}", null, null, $this->smarty);
        $this->assertEquals("0112", $this->smarty->fetch($tpl));
        $tpl2 = $this->smarty->createTemplate("eval:{\$foo}", null, null, $this->smarty);
        $this->assertEquals("1", $this->smarty->fetch($tpl2));
    }

    public function testAssignArrayAppend3()
    {
        $this->smarty->assign('foo', 1);
        $tpl = $this->smarty->createTemplate("eval:{\$foo[]=2 scope=root}{foreach \$foo as \$x}{\$x@key}{\$x}{/foreach}", null, null, $this->smarty);
        $this->assertEquals("0112", $this->smarty->fetch($tpl));
        $tpl2 = $this->smarty->createTemplate("eval:{foreach \$foo as \$x}{\$x@key}{\$x}{/foreach}", null, null, $this->smarty);
        $this->assertEquals("0112", $this->smarty->fetch($tpl2));
    }

    public function testAssignNestedArray()
    {
        $tpl = $this->smarty->createTemplate("eval:{\$foo['a'][4]=1}{\$foo['a'][4]}");
        $this->assertEquals("1", $this->smarty->fetch($tpl));
    }
}
