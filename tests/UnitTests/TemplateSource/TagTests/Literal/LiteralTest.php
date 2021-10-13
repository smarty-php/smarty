<?php
/**
 * Smarty PHPunit tests appendByRef method
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for appendByRef tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class LiteralTest extends PHPUnit_Smarty
{
    public function setUp(): void
    {
        $this->setUpSmarty(dirname(__FILE__));
    }


    public function testInit()
    {
        $this->cleanDirs();
    }
    /*
     *  Test literal tag
     */
    public function testLiteralTag()
    {
        $tpl = $this->smarty->createTemplate("string:{literal} {\$foo} {/literal}");
        $this->assertEquals(' {$foo} ', $this->smarty->fetch($tpl));
    }

    /*
    *  Test auto literal space
    */
    public function testAutoLiteralSpace()
    {
        $tpl = $this->smarty->createTemplate("string: { \$foo} ");
        $tpl->assign('foo', 'literal');
        $this->assertEquals(' { $foo} ', $this->smarty->fetch($tpl));
    }

    /*
    *  Test auto literal line break
    */
    public function testAutoLiteralLineBreak()
    {
        $tpl = $this->smarty->createTemplate("string: {\n\$foo} ");
        $tpl->assign('foo', 'literal');
        $this->assertEquals(" {\n\$foo} ", $this->smarty->fetch($tpl));
    }

    /*
    *  Test auto literal disabled
    */
    public function testAutoLiteralDisabled()
    {
        $this->smarty->setAutoLiteral(false);
        $tpl = $this->smarty->createTemplate("string:  { \$foo} ");
        $tpl->assign('foo', 'literal');
        $this->assertEquals('  literal ', $this->smarty->fetch($tpl));
    }
}
