<?php
/**
 * Smarty PHPunit tests compilation of {setfilter} tag
 *

 * @author  Uwe Tews
 */

/**
 * class for {setfilter} tag tests
 *
 * 
 * 
 * 
 */
class CompileSetfilterTest extends PHPUnit_Smarty
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
     * @run
     * InSeparateProcess
     * 
     */
    public function testNestedSetfilter()
    {
        $this->smarty->setCaching(1);
        $tpl = $this->smarty->createTemplate('string:{$foo}{setfilter escape} {$foo}{$foo nocache}{setfilter escape:"mail"} {$foo}{$foo nocache}{/setfilter} {$foo}{/setfilter} {$foo}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("<a@b.c> &lt;a@b.c&gt;&lt;a@b.c&gt; <a [AT] b [DOT] c><a [AT] b [DOT] c> &lt;a@b.c&gt; <a@b.c>", $this->smarty->fetch($tpl));
    }
    public function testNestedSetfilter1()
    {
        $this->smarty->setCaching(1);
        $tpl = $this->smarty->createTemplate('string:{$foo}{setfilter escape} {$foo}{$foo nocache}{setfilter escape:"mail"} {$foo}{$foo nocache}{/setfilter} {$foo}{/setfilter} {$foo}');
        $tpl->assign('foo', '<e@f.d>');
        $this->assertEquals("<a@b.c> &lt;a@b.c&gt;&lt;e@f.d&gt; <a [AT] b [DOT] c><e [AT] f [DOT] d> &lt;a@b.c&gt; <a@b.c>", $this->smarty->fetch($tpl));
    }
    /**
     * 
     * 
     */
    public function testNestedSetfilter2()
    {
        $tpl = $this->smarty->createTemplate('string:{$foo}{setfilter escape} {$foo}{setfilter escape:"mail"} {$foo}{/setfilter} {$foo}{/setfilter} {$foo}');
        $tpl->assign('foo', '<a@b.c>');
        $this->assertEquals("<a@b.c> &lt;a@b.c&gt; <a [AT] b [DOT] c> &lt;a@b.c&gt; <a@b.c>", $this->smarty->fetch($tpl));
    }
}
