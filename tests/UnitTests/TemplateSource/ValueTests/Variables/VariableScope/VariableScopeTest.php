<?php
/**
 * Smarty PHPunit tests variable scope
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for variable scope test
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class VariableScopeTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(__DIR__);

        $this->smarty->assign('foo', 'bar');
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * test root variable
     */
    public function testVariableScope1_0()
    {
        $tpl = $this->smarty->createTemplate('foo.tpl', null, null, $this->smarty);
        $this->assertEquals("bar", $this->smarty->fetch($tpl));
    }

    public function testVariableScope1_1()
    {
        $this->assertEquals("bar", $this->smarty->fetch('foo.tpl', null, null, $this->smarty));
    }

    public function testVariableScope12_0()
    {
        $tpl = $this->smarty->createTemplate('foo.tpl', $this->smarty);
        $this->assertEquals("bar", $this->smarty->fetch($tpl));
    }

    public function testVariableScope12_1()
    {
        $this->assertEquals("bar", $this->smarty->fetch('foo.tpl', $this->smarty));
    }

    public function testVariableScope13()
    {
        $tpl = $this->smarty->createTemplate('foo.tpl', $this->smarty);
        $this->assertEquals("bar", $tpl->fetch());
    }

    /**
     * test root variable with data object chain
     */
    public function testVariableScope2()
    {
        $data1 = new Smarty_Data($this->smarty);
        $data2 = new Smarty_Data($data1);
        $tpl = $this->smarty->createTemplate('foo.tpl', null, null, $data2);
        $this->assertEquals("bar", $this->smarty->fetch($tpl));
    }

    public function testVariableScope22()
    {
        $data1 = new Smarty_Data($this->smarty);
        $data2 = new Smarty_Data($data1);
        $tpl = $this->smarty->createTemplate('foo.tpl', $data2);
        $this->assertEquals("bar", $this->smarty->fetch($tpl));
    }

    public function testVariableScope23()
    {
        $data1 = new Smarty_Data($this->smarty);
        $data2 = new Smarty_Data($data1);
        $tpl = $this->smarty->createTemplate('foo.tpl', $data2);
        $this->assertEquals("bar", $tpl->fetch());
    }

    /**
     * test overwrite variable with data object chain
     */
    public function testVariableScope3()
    {
        $data1 = new Smarty_Data($this->smarty);
        $data1->assign('foo', 'newvalue');
        $data2 = new Smarty_Data($data1);
        $tpl = $this->smarty->createTemplate('foo.tpl', null, null, $data2);
        // must see the new value
        $this->assertEquals("newvalue", $this->smarty->fetch($tpl));
    }

    public function testVariableScope32()
    {
        $data1 = new Smarty_Data($this->smarty);
        $data2 = new Smarty_Data($data1);
        $tpl = $this->smarty->createTemplate('foo.tpl', $data2);
        // must see the old value at root
        $this->assertEquals("bar", $this->smarty->fetch($tpl));
    }

    /**
     * test local variable not seen global
     */
    public function testVariableScope4()
    {
        $this->smarty->setErrorReporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE));
        $tpl = $this->smarty->createTemplate("eval:{\$foo2='localvar'}{\$foo2}", null, null, $this->smarty);
        // must see local value
        $this->assertEquals("localvar", $this->smarty->fetch($tpl));
        // must see $foo2
        $tpl2 = $this->smarty->createTemplate("eval:{\$foo2}", null, null, $this->smarty);
        $this->assertEquals("", $this->smarty->fetch($tpl2));
    }

    public function testVariableScope42()
    {
        $this->smarty->setErrorReporting(error_reporting() & ~(E_NOTICE | E_USER_NOTICE));
        $tpl = $this->smarty->createTemplate("eval:{\$foo2='localvar'}{\$foo2}", null, null, $this->smarty);
        // must see local value
        $this->assertEquals("localvar", $this->smarty->fetch($tpl));
        // must see $foo2
        $tpl2 = $this->smarty->createTemplate("eval:{\$foo2}", $this->smarty);
        $this->assertEquals("", $this->smarty->fetch($tpl2));
    }

    /**
     * test overwriting by global variable
     */
    public function testVariableScope5()
    {
        // create variable $foo2
        $this->smarty->assign('foo2', 'oldvalue');
        $tpl = $this->smarty->createTemplate("eval:{assign var=foo2 value='newvalue' scope=parent}{\$foo2}", null, null, $this->smarty);
        // must see the new value
        $this->assertEquals("newvalue", $this->smarty->fetch($tpl));
        $tpl2 = $this->smarty->createTemplate("eval:{\$foo2}", null, null, $this->smarty);
        // must see the new value at root
        $this->assertEquals("newvalue", $this->smarty->fetch($tpl2));
    }

    public function testVariableScope52()
    {
        // create variable $foo2
        $this->smarty->assign('foo2', 'oldvalue');
        $tpl = $this->smarty->createTemplate("eval:{assign var=foo2 value='newvalue' scope=parent}{\$foo2}", null, null, $this->smarty);
        // must see the new value
        $this->assertEquals("newvalue", $this->smarty->fetch($tpl));
        $tpl2 = $this->smarty->createTemplate("eval:{\$foo2}", $this->smarty);
        // must see the new value at root
        $this->assertEquals("newvalue", $this->smarty->fetch($tpl2));
    }

    /**
     * test creation of global variable in outerscope
     */
    public function testVariableScope6()
    {
        // create global variable $foo2 in template
        $tpl = $this->smarty->createTemplate("eval:{assign var=foo2 value='newvalue' scope=parent}{\$foo2}", null, null, $this->smarty);
        // must see the new value
        $this->assertEquals("newvalue", $this->smarty->fetch($tpl));
        $tpl2 = $this->smarty->createTemplate("eval:{\$foo2}", null, null, $this->smarty);
        // must see the new value at root
        $this->assertEquals("newvalue", $this->smarty->fetch($tpl2));
    }

    public function testVariableScope62()
    {
        // create global variable $foo2 in template
        $tpl = $this->smarty->createTemplate("eval:{assign var=foo2 value='newvalue' scope=parent}{\$foo2}", null, null, $this->smarty);
        // must see the new value
        $this->assertEquals("newvalue", $this->smarty->fetch($tpl));
        $tpl2 = $this->smarty->createTemplate("eval:{\$foo2}", $this->smarty);
        // must see the new value at root
        $this->assertEquals("newvalue", $this->smarty->fetch($tpl2));
    }

    public function testDataArray()
    {
        // create global variable $foo2 in template
        $tpl = $this->smarty->createTemplate("eval:{\$foo} {\$foo2}", array('foo' => 'bar', 'foo2' => 'bar2'));
        $this->assertEquals("bar bar2", $this->smarty->fetch($tpl));
    }

    public function testDataArray2()
    {
        // create global variable $foo2 in template
        $this->assertEquals("bar bar2", $this->smarty->fetch("eval:{\$foo} {\$foo2}", array('foo' => 'bar', 'foo2' => 'bar2')));
    }

    public function testAssigns()
    {
        $expected = " local  local  local  parent root global parent root global parent root global";
        $result = $this->smarty->fetch('assign.tpl');
        $this->assertEquals($expected, $result);
    }

    public function testAssignsMerge()
    {
        $this->smarty->setMergeCompiledIncludes(true);
        $expected = " local  local  local  parent root global parent root global parent root global";
        $result = $this->smarty->fetch('assign.tpl');
        $this->assertEquals($expected, $result);
    }
}
