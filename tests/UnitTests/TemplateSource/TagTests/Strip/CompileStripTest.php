<?php
/**
 * Smarty PHPunit tests compilation of strip tags
 *
 * @package PHPunit
 * @author  Uwe Tews
 */

/**
 * class for strip tags tests
 *
 * @runTestsInSeparateProcess
 * @preserveGlobalState disabled
 * @backupStaticAttributes enabled
 */
class CompileStripTest extends PHPUnit_Smarty
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
     * test strip tag
     */
    public function testStripTag()
    {
        $this->assertContains('foobar  buh', $this->smarty->fetch('strip.tpl'));
    }
    /**
     * test strip tag multi line html
     */
    public function testStripMultiLineHtmlTag()
    {
        $this->assertContains('<div style="float: right; cursor: url;">[<a onmouseover="this.style.cursor=\'pointer\'" onmouseup="document.getElementById(\'screenEdit_($screen.id)\').style.display=\'none\'";>X</a>]</div>foobar', $this->smarty->fetch('strip_multi_line_html_tag.tpl'));
    }
    /**
     * test strip tag multi line html
     */
    public function testStripMultiLineTextareaHtmlTag()
    {
        $this->assertContains(preg_replace('/[\r]/', '', '<textarea>

                some text

            </textarea>   foobar'), $this->smarty->fetch('strip_multi_line_textarea_html_tag.tpl'));
    }
}
