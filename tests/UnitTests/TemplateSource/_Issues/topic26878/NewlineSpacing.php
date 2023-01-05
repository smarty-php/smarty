<?php
/**
 * Smarty PHPunit tests compiler errors
 *

 * @author  Uwe Tews
 */

/**
 * class for compiler tests
 *
 * 
 * @preserveGlobalState    disabled
 * 
 */
class NewlineSpacing extends PHPUnit_Smarty
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
     * Test spacings
     *
     * 
     * @dataProvider        dataTestSpacing
     * 
     */
    public function testSpacing($code, $result, $testName, $testNumber)
    {
        $name = empty($testName) ? $testNumber : $testName;
        $file = "Spacing_{$name}.tpl";
        $this->makeTemplateFile($file, $code);
        $this->smarty->setTemplateDir('./templates_tmp');
        $this->smarty->assign('file', $file);
        $this->smarty->assign('foo', 'bar');
        $this->assertEquals($result,
                            $this->smarty->fetch($file),
                            $file);
    }

    /*
      * Data provider für testSpacing
      */
    public function dataTestSpacing()
    {
        $i = 1;
        /*
                    * Code
                    * result
                    * test name
                    * test number
                    */
        return array(
            array("=====================\n{if true}\n{foreach from=array(1) item='i'}\n    <htmltag />\n{/foreach}\n{/if}\n=====================", "=====================\n    <htmltag />\n=====================", 'T1', $i++),
            array("=====================\n{if true}\n{if true}\n    <htmltag />\n{/if}\n{/if}\n=====================", "=====================\n    <htmltag />\n=====================", 'T2', $i++),
            array("=====================\n{* comment *}\n{* comment *}\n    <htmltag />\n{* comment *}\n{* comment *}\n=====================", "=====================\n    <htmltag />\n=====================", 'T3', $i++),
            array("=====================\na\n{* comment 1 *}\n{* comment 2 *}\n{* comment 3 *}\nb\n=====================", "=====================\na\nb\n=====================", 'T4', $i++),
            array("=====================\na\nb{if true}\nd<span></span>\nf{/if}\nh\n=====================", "=====================\na\nbd<span></span>\nfh\n=====================", 'T5', $i++),
        );
    }

}