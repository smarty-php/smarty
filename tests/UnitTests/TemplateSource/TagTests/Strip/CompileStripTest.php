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
        $this->setUpSmarty(dirname(__FILE__));
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
    public function testStripHtmlTag()
    {
        $this->assertContains('<ul><li><a href="#">BlaBla</a></li><li><a href="#">BlaBla</a></li></ul>', $this->smarty->fetch('strip_html_tag.tpl'));
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

    /**
     * test strip tag output tag
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testStripOutputTag()
    {
        $this->assertEquals('<h1>1 <em>italic</em></h1>', $this->smarty->fetch('strip_with_output_tag.tpl'));
    }

    /**
     * test strip tag no output tag
     *
     * @runInSeparateProcess
     * @preserveGlobalState disabled
     */
    public function testStripNoOutputTag()
    {
        $this->assertEquals('<h1><em>italic</em></h1>', $this->smarty->fetch('strip_with_no_output_tag.tpl'));
    }
}
