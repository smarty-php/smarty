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
 * @preserveGlobalState    disabled
 * @backupStaticAttributes enabled
 */
class CompileStripTest extends PHPUnit_Smarty
{
    public function setUp()
    {
        $this->setUpSmarty(dirname(__FILE__));
        $this->smarty->addPluginsDir("../../../__shared/PHPunitplugins/");
        $this->smarty->addTemplateDir("./templates_tmp");
    }

    public function testInit()
    {
        $this->cleanDirs();
    }

    /**
     * Test {strip} tags
     *
     * @not                 runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestStrip
     */
    public function testStrip($code, $result, $testName, $testNumber)
    {
        $file = "testStrip_{$testNumber}.tpl";
        $this->makeTemplateFile($file, "{strip}\n" . $code);
        $this->smarty->assignGlobal('file', $file);
        $this->assertEquals($result, $this->smarty->fetch($file), "testStrip - {$code} - {$testName}");
    }

    /*
      * Data provider für testStrip
      */
    public function dataTestStrip()
    {
        $i = 0;
        /*
                    * Code
                    * result
                    * test name
                    */
        return array(array("    foo\n    bar  buh\n\n", 'foobar  buh', '', $i ++),
                     array("\n   <div style=\"float: right; cursor: url;\">[<a\n    onmouseover=\"this.style.cursor='pointer'\"\n    onmouseup=\"document.getElementById('screenEdit_(\$screen.id)').style.display='none'\">X</a>]</div>\n\n\n   foo\n    bar\n",
                           '<div style="float: right; cursor: url;">[<a onmouseover="this.style.cursor=\'pointer\'" onmouseup="document.getElementById(\'screenEdit_($screen.id)\').style.display=\'none\'">X</a>]</div>foobar',
                           '', $i ++),
                     array("\n    <ul>\n        <li>\n            <a href=\"#\">BlaBla</a>\n        </li>\n        <li>\n            <a href=\"#\">BlaBla</a>\n        </li>\n    </ul>\n",
                           '<ul><li><a href="#">BlaBla</a></li><li><a href="#">BlaBla</a></li></ul>', '', $i ++),
                     array("\n            <textarea>\n\n                some text\n\n            </textarea>   foo\n    bar\n",
                           "<textarea>\n\n                some text\n\n            </textarea>   foobar", '', $i ++),
                     // variable in html tag
                     array("\n    <b><c>c</c></b>\n", '<b><c>c</c></b>', '', $i ++),
                     array("\n    <b> <c>c</c></b>\n", '<b> <c>c</c></b>', '', $i ++),
                     array("\n    <b>\n<c>c</c></b>\n", '<b><c>c</c></b>', '', $i ++),
                     array("{\$foo=1}\n    <b>{\$foo}<c>c</c></b>\n", '<b>1<c>c</c></b>', '', $i ++),
                     array("{\$foo=1}\n    <b>{\$foo} <c>c</c></b>\n", '<b>1 <c>c</c></b>', '', $i ++),
                     array("{\$foo=1}\n    <b>\n{\$foo} <c>c</c></b>\n", '<b>1 <c>c</c></b>', '', $i ++),
                     array("\n<span>#</span>{'Text'}<span>#</span>\n", '<span>#</span>Text<span>#</span>', '', $i ++),
                     array("\n<span>#</span> {'Text'}\n", '<span>#</span> Text', '', $i ++),
                     array("\n<span>#</span> {'Text'}\n", '<span>#</span> Text', '', $i ++),
                     array("\n<b></b><c></c>", '<b></b><c></c>', '', $i ++),
                     array("{'Var'}\n<b></b> <c></c>", 'Var<b></b> <c></c>', '', $i ++),
                     array("{'Var'}\n <b></b> <c></c>", 'Var<b></b> <c></c>', '', $i ++),
                     array("\n<b></b>  <c></c>", '<b></b> <c></c>', '', $i ++),
                     array("\n<b></b>\n  <c></c>", '<b></b><c></c>', '', $i ++),
                     array("\n<b>\n  {* a comment *}\n   <c>", '<b><c>', '', $i ++),
        );
    }

}
