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
     * @not runInSeparateProcess
     * @preserveGlobalState disabled
     * @dataProvider        dataTestStrip
     */
    public function testStrip($code, $result, $testName, $testNumber)
    {
        $file = "testStrip_{$testNumber}.tpl";
        $this->makeTemplateFile($file, "{strip}\n" . $code);
        $this->smarty->assignGlobal('file', $file);
        $this->assertEquals($this->strip($result), $this->strip($this->smarty->fetch($file)), "testStrip - {$code} - {$testName}");
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
        return array(
                     array("    foo\n    bar  buh\n\n", 'foobar  buh', '', $i ++),
                     array("\n   <div style=\"float: right; cursor: url;\">[<a\n    onmouseover=\"this.style.cursor='pointer'\"\n    onmouseup=\"document.getElementById('screenEdit_(\$screen.id)').style.display='none'\";>X</a>]</div>\n\n\n   foo\n    bar\n", '<div style="float: right; cursor: url;">[<a onmouseover="this.style.cursor=\'pointer\'" onmouseup="document.getElementById(\'screenEdit_($screen.id)\').style.display=\'none\'";>X</a>]</div>foobar', '', $i ++),
                     array("\n    <ul>\n        <li>\n            <a href=\"#\">BlaBla</a>\n        </li>\n        <li>\n            <a href=\"#\">BlaBla</a>\n        </li>\n    </ul>\n", '<ul><li><a href="#">BlaBla</a></li><li><a href="#">BlaBla</a></li></ul>', '', $i ++),
                     array("\n            <textarea>\n\n                some text\n\n            </textarea>   foo\n    bar\n", "<textarea>\n\n                some text\n\n            </textarea>   foobar", '', $i ++),
                     // variable in html tag
                     array("{\$foo=1}\n    <h1>{getvar var=foo} <em>italic</em></h1>\n", '<h1>1 <em>italic</em></h1>', '', $i ++),
                     array("{\$foo=1}\n    <h1>{getvar var=foo assign=newvar} <em>italic</em></h1>\n", '<h1><em>italic</em></h1>', '', $i ++),
                     array("{\$text=\"Text\"}\n<span>#</span>{\$text}<span>#</span>\n", '<span>#</span>Text<span>#</span>', '', $i ++),
                     array("{\$text=\"Text\"}\n<span>#</span> {'Text'}\n", '<span>#</span> Text', '', $i ++),

        );
    }

}
